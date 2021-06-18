{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE PackageImports #-}

module Cardano.Tracer.Acceptors
  ( runAcceptors
  ) where

import           Codec.CBOR.Term (Term)
import           Control.Concurrent (ThreadId, killThread, myThreadId)
import           Control.Concurrent.Async (async, asyncThreadId, wait, waitAnyCancel)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVarIO)
import           Control.Exception (SomeException, try)
import           Control.Monad (void)
import "contra-tracer" Control.Tracer (nullTracer)
import qualified Data.ByteString.Lazy as LBS
import           Data.IORef (IORef, newIORef, readIORef)
import           Data.HashMap.Strict ((!))
import           Data.Time.Clock (secondsToNominalDiffTime)
import           Data.Void (Void)
import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolLimits (..),
                                        MiniProtocolNum (..), MuxMode (..),
                                        OuroborosApplication (..),
                                        RunMiniProtocol (..),
                                        miniProtocolLimits, miniProtocolNum, miniProtocolRun)
import           Ouroboros.Network.Driver.Limits (ProtocolTimeLimits)
import           Ouroboros.Network.ErrorPolicy (nullErrorPolicies)
import           Ouroboros.Network.IOManager (withIOManager)
import           Ouroboros.Network.Snocket (Snocket, localAddressFromPath, localSnocket)
import           Ouroboros.Network.Socket (AcceptedConnectionsLimit (..), ConnectionId (..),
                                           SomeResponderApplication (..),
                                           cleanNetworkMutableState, newNetworkMutableState,
                                           nullNetworkServerTracers, withServerNode)
import           Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec,
                                                             noTimeLimitsHandshake)
import           Ouroboros.Network.Protocol.Handshake.Unversioned (UnversionedProtocol (..),
                                                                   UnversionedProtocolData (..),
                                                                   unversionedHandshakeCodec,
                                                                   unversionedProtocolDataCodec)
import           Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import           Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion,
                                                               simpleSingletonVersions)
import           System.IO.Unsafe (unsafePerformIO)

import           Cardano.Logging (TraceObject)

import qualified Trace.Forward.Configuration as TF
import qualified Trace.Forward.Protocol.Type as TF
import           Trace.Forward.Network.Acceptor (acceptTraceObjects)

import qualified System.Metrics.Configuration as EKGF
import qualified System.Metrics.ReqResp as EKGF
import           System.Metrics.Network.Acceptor (acceptEKGMetrics)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Types (AcceptedItems, TraceObjects, Metrics,
                                       addressToNodeId, prepareAcceptedItems)

runAcceptors
  :: TracerConfig
  -> AcceptedItems
  -> IO ()
runAcceptors config@TracerConfig{..} acceptedItems = do
  stopEKG <- newIORef False
  stopTF  <- newIORef False

  -- Temporary fill 'tidVar' using current 'ThreadId'. Later it will be
  -- replaced by the real 'ThreadId' from 'serverAsync' (see below).
  tmpTId <- myThreadId
  tidVar :: TVar ThreadId <- newTVarIO tmpTId

  let configs = mkAcceptorsConfigs config stopEKG stopTF

  try (runAcceptors' acceptAt configs tidVar acceptedItems) >>= \case
    Left (e :: SomeException) -> do
      -- There is some problem (probably the connection was dropped).
      putStrLn $ "cardano-tracer, runAcceptors problem: " <> show e
      -- Explicitly stop 'serverAsync'.
      killThread =<< readTVarIO tidVar
      runAcceptors config acceptedItems
    Right _ -> return ()

mkAcceptorsConfigs
  :: TracerConfig
  -> IORef Bool
  -> IORef Bool
  -> ( EKGF.AcceptorConfiguration
     , TF.AcceptorConfiguration TraceObject
     )
mkAcceptorsConfigs TracerConfig{..} stopEKG stopTF = (ekgConfig, tfConfig)
 where
  ekgConfig =
    EKGF.AcceptorConfiguration
      { EKGF.acceptorTracer    = nullTracer
      , EKGF.forwarderEndpoint = forEKGF acceptAt
      , EKGF.requestFrequency  = secondsToNominalDiffTime ekgRequestFreq
      , EKGF.whatToRequest     = EKGF.GetAllMetrics
      , EKGF.actionOnResponse  = print
      , EKGF.shouldWeStop      = stopEKG
      , EKGF.actionOnDone      = putStrLn "EKGF: we are done!"
      }

  tfConfig :: TF.AcceptorConfiguration TraceObject
  tfConfig =
    TF.AcceptorConfiguration
      { TF.acceptorTracer    = nullTracer
      , TF.forwarderEndpoint = forTF acceptAt
      , TF.whatToRequest     = TF.GetTraceObjects loRequestNum
      , TF.actionOnReply     = print
      , TF.shouldWeStop      = stopTF
      , TF.actionOnDone      = putStrLn "TF: we are done!"
      }

  forTF (LocalSocket p)   = TF.LocalPipe p
  forEKGF (LocalSocket p) = EKGF.LocalPipe p

runAcceptors'
  :: Address
  -> (EKGF.AcceptorConfiguration, TF.AcceptorConfiguration TraceObject)
  -> TVar ThreadId
  -> AcceptedItems
  -> IO ()
runAcceptors' (LocalSocket localSock) configs tidVar acceptedItems = withIOManager $ \iocp -> do
  let snock = localSnocket iocp localSock
      addr  = localAddressFromPath localSock
  doListenToForwarder snock addr noTimeLimitsHandshake configs tidVar acceptedItems  

doListenToForwarder
  :: (Ord addr, Show addr)
  => Snocket IO fd addr
  -> addr
  -> ProtocolTimeLimits (Handshake UnversionedProtocol Term)
  -> (EKGF.AcceptorConfiguration, TF.AcceptorConfiguration TraceObject)
  -> TVar ThreadId
  -> AcceptedItems
  -> IO ()
doListenToForwarder snocket
                    address
                    timeLimits
                    (ekgConfig, tfConfig)
                    tidVar
                    acceptedItems = do
  networkState <- newNetworkMutableState
  nsAsync <- async $ cleanNetworkMutableState networkState
  clAsync <- async . void $
    withServerNode
      snocket
      nullNetworkServerTracers
      networkState
      (AcceptedConnectionsLimit maxBound maxBound 0)
      address
      unversionedHandshakeCodec
      timeLimits
      (cborTermVersionDataCodec unversionedProtocolDataCodec)
      acceptableVersion
      (simpleSingletonVersions
        UnversionedProtocol
        UnversionedProtocolData
        (SomeResponderApplication $ acceptorApp
          [ (runEKGAcceptor        ekgConfig acceptedItems, 1)
          , (runTraceObjectsAcceptor tfConfig  acceptedItems, 2)
          ]
        )
      )
      nullErrorPolicies
      $ \_ serverAsync -> do
        -- Store 'serverAsync' to be able to kill it later.
        atomically $ modifyTVar' tidVar $ const (asyncThreadId serverAsync)
        wait serverAsync -- Block until async exception.
  void $ waitAnyCancel [nsAsync, clAsync]
 where
  acceptorApp protocols =
    OuroborosApplication $ \connectionId _shouldStopSTM ->
      [ MiniProtocol
         { miniProtocolNum    = MiniProtocolNum num
         , miniProtocolLimits = MiniProtocolLimits { maximumIngressQueue = maxBound }
         , miniProtocolRun    = protocol connectionId
         }
      | (protocol, num) <- protocols
      ]

runEKGAcceptor
  :: Show addr
  => EKGF.AcceptorConfiguration
  -> AcceptedItems
  -> ConnectionId addr
  -> RunMiniProtocol 'ResponderMode LBS.ByteString IO Void ()
runEKGAcceptor ekgConfig acceptedItems connId = do
  let (_, _, (ekgStore, localStore)) =
        unsafePerformIO $ prepareStores acceptedItems connId
  acceptEKGMetrics ekgConfig ekgStore localStore

runTraceObjectsAcceptor
  :: Show addr
  => TF.AcceptorConfiguration TraceObject
  -> AcceptedItems
  -> ConnectionId addr
  -> RunMiniProtocol 'ResponderMode LBS.ByteString IO Void ()
runTraceObjectsAcceptor tfConfig acceptedItems connId = do
  let (niStore, trObQueue, _) =
        unsafePerformIO $ prepareStores acceptedItems connId
  acceptTraceObjects tfConfig trObQueue niStore

prepareStores
  :: Show addr
  => AcceptedItems
  -> ConnectionId addr
  -> IO (TF.NodeInfoStore, TraceObjects, Metrics)
prepareStores acceptedItems ConnectionId{..} = do
  -- Remote address of the node is unique identifier, from the tracer's point of view.
  let nodeId = addressToNodeId $ show remoteAddress
  prepareAcceptedItems nodeId acceptedItems
  items <- readIORef acceptedItems
  return $ items ! nodeId
