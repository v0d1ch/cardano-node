{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Benchmarking.GeneratorTx.Submission
  ( SubmissionParams(..)
  , SubmissionThreadReport
  , TxSendQueue
  , TxSource
  , ReportRef
  , legacyTxSource
  , mkSubmissionSummary
  , submitThreadReport
  , txSubmissionClient
  , simpleTxFeeder
  , tpsLimitedTxFeeder
  , tpsLimitedTxFeederShutdown
  ) where

import           Prelude (String, error, fail)
import           Cardano.Prelude hiding (ByteString, atomically, retry, state, threadDelay)

import           Control.Arrow ((&&&))
import           Control.Concurrent (threadDelay)
import qualified Control.Concurrent.STM as STM
import           Control.Concurrent.STM.TBQueue (TBQueue)

import qualified Data.List as L
import qualified Data.List.Extra as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import           Data.Time.Clock (NominalDiffTime, UTCTime)
import qualified Data.Time.Clock as Clock

import           Control.Tracer (Tracer, traceWith)

import           Cardano.Tracing.OrphanInstances.Byron ()
import           Cardano.Tracing.OrphanInstances.Common ()
import           Cardano.Tracing.OrphanInstances.Consensus ()
import           Cardano.Tracing.OrphanInstances.Network ()
import           Cardano.Tracing.OrphanInstances.Shelley ()

import qualified Ouroboros.Consensus.Cardano as Consensus (CardanoBlock)
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTx, GenTxId, txInBlockSize)
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as Mempool
import           Ouroboros.Consensus.Shelley.Ledger.Mempool (mkShelleyTx)
import qualified Ouroboros.Consensus.Shelley.Ledger.Mempool as Mempool (TxId(ShelleyTxId))
import           Ouroboros.Consensus.Shelley.Protocol (StandardCrypto)

import           Ouroboros.Consensus.Cardano.Block (GenTx (GenTxAllegra, GenTxAlonzo, GenTxShelley, GenTxMary))
import qualified Ouroboros.Consensus.Cardano.Block as Block (TxId(GenTxIdShelley, GenTxIdAllegra, GenTxIdMary))

import           Ouroboros.Network.Protocol.TxSubmission.Client (ClientStIdle (..),
                                                                 ClientStTxIds (..),
                                                                 ClientStTxs (..),
                                                                 TxSubmissionClient (..))
import           Ouroboros.Network.Protocol.TxSubmission.Type (BlockingReplyList (..),
                                                               TokBlockingStyle (..), TxSizeInBytes)

import           Cardano.Api
import           Cardano.Api.Shelley (Tx(ShelleyTx), fromShelleyTxId)

import           Cardano.Benchmarking.Tracer
import           Cardano.Benchmarking.Types

{-------------------------------------------------------------------------------
  Parametrisation & state
-------------------------------------------------------------------------------}
type CardanoBlock    = Consensus.CardanoBlock  StandardCrypto

data SubmissionParams
  = SubmissionParams
      { spTps           :: !TPSRate
      , spTargets       :: !Natural
      }

type ReportRef = STM.TMVar (Either String SubmissionThreadReport)
type TxSendQueue era = TBQueue (Maybe (Tx era))

submitThreadReport
  :: MonadIO m
  => ReportRef
  -> Either String SubmissionThreadReport
  -> m ()
submitThreadReport ref report
 = liftIO $ STM.atomically $ STM.putTMVar ref report

{-------------------------------------------------------------------------------
  Results
-------------------------------------------------------------------------------}
data SubmissionThreadStats
  = SubmissionThreadStats
      { stsAcked       :: {-# UNPACK #-} !Ack
      , stsSent        :: {-# UNPACK #-} !Sent
      , stsUnavailable :: {-# UNPACK #-} !Unav
      }

data SubmissionThreadReport
  = SubmissionThreadReport
      { strStats         :: !SubmissionThreadStats
      , strEndOfProtocol :: !UTCTime
      }

mkSubmissionSummary ::
     String
  -> UTCTime
  -> [ReportRef]
  -> IO SubmissionSummary
mkSubmissionSummary ssThreadName startTime reportsRefs
 = do
  results <- sequence (STM.atomically . STM.readTMVar <$> reportsRefs)
  let (failures, reports) = partitionEithers results
  now <- Clock.getCurrentTime
  let ssElapsed = Clock.diffUTCTime now startTime
      ssTxSent@(Sent sent) = sum $ stsSent . strStats <$> reports
      ssTxUnavailable = sum $ stsUnavailable . strStats <$> reports
      ssEffectiveTps = txDiffTimeTPS sent ssElapsed
      ssThreadwiseTps = threadReportTps <$> reports
      ssFailures = failures
  pure SubmissionSummary{..}
 where
  txDiffTimeTPS :: Int -> NominalDiffTime -> TPSRate
  txDiffTimeTPS n delta =
    TPSRate $ realToFrac $ fromIntegral n / delta

  threadReportTps :: SubmissionThreadReport -> TPSRate
  threadReportTps
    SubmissionThreadReport
      { strStats=SubmissionThreadStats{stsAcked=Ack ack}, strEndOfProtocol } =
        txDiffTimeTPS ack (Clock.diffUTCTime strEndOfProtocol startTime)

{-------------------------------------------------------------------------------
  Submission queue:  feeding and consumption
-------------------------------------------------------------------------------}
simpleTxFeeder :: forall m era .
     MonadIO m
  => Tracer m (TraceBenchTxSubmit TxId)
  -> Natural
  -> TxSendQueue era
  -> [Tx era]
  -> m ()
simpleTxFeeder tracer threads txSendQueue txs = do
  forM_ (zip txs [0..]) feedTx
  -- Issue the termination notifications.
  replicateM_ (fromIntegral threads) $
    liftIO $ STM.atomically $ STM.writeTBQueue txSendQueue Nothing
 where
  feedTx :: (Tx era, Int) -> m ()
  feedTx (tx, ix) = do
    liftIO $ STM.atomically $ STM.writeTBQueue txSendQueue (Just tx)
    traceWith tracer $ TraceBenchTxSubServFed [getTxId $ getTxBody tx] ix

tpsLimitedTxFeederShutdown ::
     Natural
  -> TxSendQueue era
  -> IO ()
tpsLimitedTxFeederShutdown threads txSendQueue
   = STM.atomically $
       replicateM_ (fromIntegral threads)
          $ STM.writeTBQueue txSendQueue Nothing

tpsLimitedTxFeeder :: forall m era .
     MonadIO m
  => Tracer m (TraceBenchTxSubmit TxId)
  -> Natural
  -> TxSendQueue era
  -> TPSRate
  -> [Tx era] -> m ()
tpsLimitedTxFeeder tracer threads txSendQueue (TPSRate rate) txs = do
  -- It would be nice to catch an AsyncException here and do a clean shutdown.
  -- However this would require extra machineries because we are in MonadIO m not in IO ().
  -- TODO: Move everything to IO () and avoid problems from over-polymorphism.
  now <- liftIO Clock.getCurrentTime
  foldM_ feedTx (now, 0) (zip txs [0..])
  liftIO $ tpsLimitedTxFeederShutdown threads txSendQueue
 where

  feedTx :: (UTCTime, NominalDiffTime)
         -> (Tx era, Int)
         -> m (UTCTime, NominalDiffTime)
  feedTx (lastPreDelay, lastDelay) (tx, ix) = do
    liftIO . STM.atomically $ STM.writeTBQueue txSendQueue (Just tx)
    traceWith tracer $ TraceBenchTxSubServFed [getTxId $ getTxBody tx] ix
    now <- liftIO Clock.getCurrentTime
    let targetDelay = realToFrac $ 1.0 / rate
        loopCost = (now `Clock.diffUTCTime` lastPreDelay) - lastDelay
        delay = targetDelay - loopCost
    liftIO . threadDelay . ceiling $ (realToFrac delay * 1000000.0 :: Double)
    pure (now, delay)

data TxSource era
  = Exhausted
  | Active (ProduceNextTxs era)

type ProduceNextTxs era = (forall m blocking . MonadIO m => TokBlockingStyle blocking -> Req -> m (TxSource era, [Tx era]))

type LocalState era = (TxSource era, UnAcked (Tx era), SubmissionThreadStats)

produceNextTxs :: forall m blocking era . MonadIO m => TokBlockingStyle blocking -> Req -> LocalState era -> m (LocalState era, [Tx era])
produceNextTxs blocking req (txProducer, unack, stats) = do
  (newTxProducer, txList) <- produceNextTxs' blocking req txProducer
  return ((newTxProducer, unack, stats), txList)

produceNextTxs' :: forall m blocking era . MonadIO m => TokBlockingStyle blocking -> Req -> TxSource era -> m (TxSource era, [Tx era])
produceNextTxs' _ _ Exhausted = return (Exhausted, [])
produceNextTxs' blocking req (Active callback) = callback blocking req

-- This is used in the two phase/ non wallet based tx-generator.
legacyTxSource :: forall era. TxSendQueue era -> TxSource era
legacyTxSource txSendQueue = Active worker
 where
  worker :: forall m blocking . MonadIO m => TokBlockingStyle blocking -> Req -> m (TxSource era, [Tx era])
  worker blocking req = do
    (done, txList) <- case blocking of
       TokBlocking -> consumeTxsBlocking req
       TokNonBlocking -> consumeTxsNonBlocking req
    if done
       then return (Exhausted, txList)
       else return (Active worker, txList)

  consumeTxsBlocking ::
       MonadIO m
    => Req -> m (Bool, [Tx era])
  consumeTxsBlocking req
    = liftIO . STM.atomically $ go req []
   where
    go :: Req -> [Tx era] -> STM (Bool, [Tx era])
    go 0 acc = pure (False, acc)
    go n acc = STM.readTBQueue txSendQueue >>=
      \case
        Nothing -> pure (True, acc)
        Just tx -> go (n - 1) (tx:acc)

  consumeTxsNonBlocking ::
       MonadIO m
    => Req -> m (Bool, [Tx era])
  consumeTxsNonBlocking req
    = liftIO . STM.atomically $
        if req==0 then pure (False, [])
          else do
            STM.tryReadTBQueue txSendQueue >>= \case
              Nothing -> pure (False, [])
              Just Nothing -> pure (True, [])
              Just (Just tx) -> pure (False, [tx])

txSubmissionClient
  :: forall m era tx txid gentx gentxid .
     ( MonadIO m, MonadFail m
     , IsShelleyBasedEra era
     , tx      ~ Tx era
     , txid    ~ TxId
     , gentx   ~ GenTx CardanoBlock
     , gentxid ~ GenTxId CardanoBlock
     )
  => Tracer m NodeToNodeSubmissionTrace
  -> Tracer m (TraceBenchTxSubmit txid)
  -> TxSource era
  -> ReportRef
  -- This return type is forced by Ouroboros.Network.NodeToNode.connectTo
  -> TxSubmissionClient gentxid gentx m ()
txSubmissionClient tr bmtr initialTxSource reportRef =
  TxSubmissionClient $
    pure $ client (initialTxSource, UnAcked [], SubmissionThreadStats 0 0 0)
 where
  discardAcknowledged :: TokBlockingStyle a -> Ack -> LocalState era -> m (LocalState era)
  discardAcknowledged blocking (Ack ack) (txSource, UnAcked unAcked, stats) = do
    when (tokIsBlocking blocking && ack /= length unAcked) $ do
      let err = "decideAnnouncement: TokBlocking, but length unAcked != ack"
      traceWith bmtr (TraceBenchTxSubError err)
      fail (T.unpack err)
    let (stillUnacked, acked) = L.splitAtEnd ack unAcked
    let newStats = stats { stsAcked = stsAcked stats + Ack ack }
    traceWith bmtr $ TraceBenchTxSubServAck  (getTxId . getTxBody <$> acked)
    return (txSource, UnAcked stillUnacked, newStats)

  queueNewTxs :: [tx] -> LocalState era -> LocalState era
  queueNewTxs newTxs (txSource, UnAcked unAcked, stats)
    = (txSource, UnAcked (newTxs <> unAcked), stats)

  -- Sadly, we can't just return what we want, we instead have to
  -- communicate via IORefs, because..
  -- The () return type is forced by Ouroboros.Network.NodeToNode.connectTo
  client ::LocalState era -> ClientStIdle gentxid gentx m ()

  client localState = ClientStIdle
    { recvMsgRequestTxIds = requestTxIds localState
    , recvMsgRequestTxs = requestTxs localState
    }

  requestTxIds :: forall blocking.
       LocalState era
    -> TokBlockingStyle blocking
    -> Word16
    -> Word16
    -> m (ClientStTxIds blocking gentxid gentx m ())
  requestTxIds state blocking ackNum reqNum = do
    let ack = Ack $ fromIntegral ackNum
        req = Req $ fromIntegral reqNum
    traceWith tr $ reqIdsTrace ack req blocking
    stateA <- discardAcknowledged blocking ack state

    (stateB, newTxs) <- produceNextTxs blocking req stateA
    let stateC@(_, UnAcked outs , stats) = queueNewTxs newTxs stateB

    traceWith tr $ idListTrace (ToAnnce newTxs) blocking
    traceWith bmtr $ TraceBenchTxSubServAnn  (getTxId . getTxBody <$> newTxs)
    traceWith bmtr $ TraceBenchTxSubServOuts (getTxId . getTxBody <$> outs)

    case blocking of
      TokBlocking -> case NE.nonEmpty newTxs of
        Nothing -> do
          traceWith tr EndOfProtocol
          _wantedReturnValue <- submitReport stats
          pure $ SendMsgDone ()
        (Just txs) -> pure $ SendMsgReplyTxIds
                              (BlockingReply $ txToIdSize <$> txs)
                              (client stateC)
      TokNonBlocking ->  pure $ SendMsgReplyTxIds
                             (NonBlockingReply $ txToIdSize <$> newTxs)
                             (client stateC)
                    
  requestTxs ::
       LocalState era
    -> [GenTxId CardanoBlock]
    -> m (ClientStTxs (GenTxId CardanoBlock) (GenTx CardanoBlock) m ())
  requestTxs (txSource, unAcked, stats) txIds = do
    let  reqTxIds :: [txid]
         reqTxIds = fmap fromGenTxId txIds
    traceWith tr $ ReqTxs (length reqTxIds)
    let UnAcked ua = unAcked
        uaIds = getTxId . getTxBody <$> ua
        (toSend, _retained) = L.partition ((`L.elem` reqTxIds) . getTxId . getTxBody) ua
        missIds = reqTxIds L.\\ uaIds

    traceWith tr $ TxList (length toSend)
    traceWith bmtr $ TraceBenchTxSubServReq reqTxIds
    traceWith bmtr $ TraceBenchTxSubServOuts (getTxId . getTxBody <$> ua)
    unless (L.null missIds) $
      traceWith bmtr $ TraceBenchTxSubServUnav missIds
    pure $ SendMsgReplyTxs (toGenTx <$> toSend)
      (client (txSource, unAcked,
        stats { stsSent =
                stsSent stats + Sent (length toSend)
              , stsUnavailable =
                stsUnavailable stats + Unav (length missIds)}))

  submitReport :: SubmissionThreadStats -> m SubmissionThreadReport
  submitReport strStats = do
    strEndOfProtocol <- liftIO Clock.getCurrentTime
    let report = SubmissionThreadReport{..}
    submitThreadReport reportRef (Right report)
    pure report

  txToIdSize :: tx -> (gentxid, TxSizeInBytes)
  txToIdSize = (Mempool.txId &&& txInBlockSize) . toGenTx

  toGenTx :: tx -> gentx
  toGenTx tx = case (shelleyBasedEra @ era , tx) of
    (ShelleyBasedEraShelley, ShelleyTx _ tx') -> GenTxShelley (mkShelleyTx tx')
    (ShelleyBasedEraAllegra, ShelleyTx _ tx') -> GenTxAllegra (mkShelleyTx tx')
    (ShelleyBasedEraMary, ShelleyTx _ tx') -> GenTxMary (mkShelleyTx tx')
    (ShelleyBasedEraAlonzo, ShelleyTx _ tx') -> GenTxAlonzo (mkShelleyTx tx')

  fromGenTxId :: gentxid -> txid
  fromGenTxId (Block.GenTxIdShelley (Mempool.ShelleyTxId i)) = fromShelleyTxId i
  fromGenTxId (Block.GenTxIdAllegra (Mempool.ShelleyTxId i)) = fromShelleyTxId i
  fromGenTxId (Block.GenTxIdMary    (Mempool.ShelleyTxId i)) = fromShelleyTxId i
  fromGenTxId _ = error "submission.hs: fromGenTxId"

  tokIsBlocking :: TokBlockingStyle a -> Bool
  tokIsBlocking = \case
    TokBlocking    -> True
    TokNonBlocking -> False

  reqIdsTrace :: Ack -> Req -> TokBlockingStyle a -> NodeToNodeSubmissionTrace
  reqIdsTrace ack req = \case
     TokBlocking    -> ReqIdsBlocking ack req
     TokNonBlocking -> ReqIdsPrompt   ack req

  idListTrace :: ToAnnce tx -> TokBlockingStyle a -> NodeToNodeSubmissionTrace
  idListTrace (ToAnnce toAnn) = \case
     TokBlocking    -> IdsListBlocking $ length toAnn
     TokNonBlocking -> IdsListPrompt   $ length toAnn
