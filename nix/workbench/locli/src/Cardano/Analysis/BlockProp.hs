{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-partial-fields -Wno-unused-matches -Wno-deprecations -Wno-unused-local-binds -Wno-incomplete-record-updates #-}
module Cardano.Analysis.BlockProp (module Cardano.Analysis.BlockProp) where

import           Prelude (String, (!!), error, head, id, show, tail)
import           Cardano.Prelude hiding (head, show)

import           Control.Arrow ((***))
import           Control.Concurrent.Async (mapConcurrently)
import           Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Data.Aeson as AE
import           Data.Bifunctor
import           Data.Function (on)
import           Data.List (dropWhileEnd, intercalate)
import           Data.Maybe (catMaybes, mapMaybe)
import           Data.Tuple (swap)
import           Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Data.Map.Strict as Map

import           Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)

import           Text.Printf (printf)

import           Ouroboros.Network.Block (BlockNo(..), SlotNo(..))

import           Data.Accum
import           Data.Distribution
import           Cardano.Profile
import           Cardano.Unlog.LogObject hiding (Text)
import           Cardano.Unlog.Render
import           Cardano.Unlog.Resources
import           Cardano.Unlog.SlotStats

import qualified Debug.Trace as D


data BlockPropagation
  = BlockPropagation
    { bpForgerForges        :: !(Distribution Float NominalDiffTime)
    , bpForgerAdoptions     :: !(Distribution Float NominalDiffTime)
    , bpForgerAnnouncements :: !(Distribution Float NominalDiffTime)
    , bpForgerSends         :: !(Distribution Float NominalDiffTime)
    , bpPeerNotices         :: !(Distribution Float NominalDiffTime, Distribution Float NominalDiffTime)
    , bpPeerFetches         :: !(Distribution Float NominalDiffTime, Distribution Float NominalDiffTime)
    , bpPeerAdoptions       :: !(Distribution Float NominalDiffTime, Distribution Float NominalDiffTime)
    , bpPeerAnnouncements   :: !(Distribution Float NominalDiffTime, Distribution Float NominalDiffTime)
    , bpPeerSends           :: !(Distribution Float NominalDiffTime, Distribution Float NominalDiffTime)
    , bpChainBlockEvents    :: [BlockEvents]
    }
  deriving Show

instance RenderDistributions BlockPropagation where
  rdFields =
    --  Width LeftPad
    [ Field 6 0 "forged"        (f!!0) "Forge"   $ DDeltaT bpForgerForges
    , Field 6 0 "fAdopted"      (f!!1) "Adopt"   $ DDeltaT bpForgerAdoptions
    , Field 6 0 "fAnnounced"    (f!!2) "Announ"  $ DDeltaT bpForgerAnnouncements
    , Field 6 0 "fSendStart"    (f!!3) "Sendin"  $ DDeltaT bpForgerSends
    , Field 4 1 "noticedVal"    (p!!0) " Noti"   $ DDeltaT (fst . bpPeerNotices)
    , Field 4 0 "noticedCoV"    (p!!1) "ced  "   $ DDeltaT (snd . bpPeerNotices)
    , Field 4 1 "fetchedVal"    (p!!2) " Fetc"   $ DDeltaT (fst . bpPeerFetches)
    , Field 4 0 "fetchedCoV"    (p!!3) "hed  "   $ DDeltaT (snd . bpPeerFetches)
    , Field 4 1 "pAdoptedVal"   (p!!4) " Adop"   $ DDeltaT (fst . bpPeerAdoptions)
    , Field 4 0 "pAdoptedCoV"   (p!!5) "ted  "   $ DDeltaT (snd . bpPeerAdoptions)
    , Field 4 1 "pAnnouncedVal" (p!!6) "Annou" $ DDeltaT (fst . bpPeerAnnouncements)
    , Field 4 0 "pAnnouncedCoV" (p!!7) "nced " $ DDeltaT (snd . bpPeerAnnouncements)
    , Field 4 1 "pSendStartVal" (p!!8) " Send" $ DDeltaT (fst . bpPeerSends)
    , Field 4 0 "pSendStartCoV" (p!!9) "ing  " $ DDeltaT (snd . bpPeerSends)
    ]
   where
     f = nChunksEachOf  4 7 "Forger event Δt:"
     p = nChunksEachOf 10 5 "Peer event Δt, and coefficients of variation:"

instance AE.ToJSON BlockPropagation where
  toJSON BlockPropagation{..} = AE.Array $ Vec.fromList
    [ extendObject "kind" "forgerForges"        $ toJSON bpForgerForges
    , extendObject "kind" "forgerAdoptions"     $ toJSON bpForgerAdoptions
    , extendObject "kind" "forgerAnnouncements" $ toJSON bpForgerAnnouncements
    , extendObject "kind" "forgerSends"         $ toJSON bpForgerSends
    , extendObject "kind" "peerNoticesMean"       $ toJSON (fst bpPeerNotices)
    , extendObject "kind" "peerNoticesCoV"        $ toJSON (snd bpPeerNotices)
    , extendObject "kind" "peerFetchesMean"       $ toJSON (fst bpPeerFetches)
    , extendObject "kind" "peerFetchesCoV"        $ toJSON (snd bpPeerFetches)
    , extendObject "kind" "peerAdoptionsMean"     $ toJSON (fst bpPeerAdoptions)
    , extendObject "kind" "peerAdoptionsCoV"      $ toJSON (snd bpPeerAdoptions)
    , extendObject "kind" "peerAnnouncementsMean" $ toJSON (fst bpPeerAnnouncements)
    , extendObject "kind" "peerAnnouncementsCoV"  $ toJSON (snd bpPeerAnnouncements)
    , extendObject "kind" "peerSendsMean"         $ toJSON (fst bpPeerSends)
    , extendObject "kind" "peerSendsCoV"          $ toJSON (snd bpPeerSends)
    ]

data BPError
  = BPError
  { eLO    :: !LogObject
  , eBlock :: !Hash
  , eFile  :: !(Maybe FilePath)
  , eDesc  :: !String
  }
  deriving (FromJSON, Generic, Show, ToJSON)

-- | Block's events, as seen by its forger.
data BlockForgerEvents
  =  BlockForgerEvents
  { bfeHost       :: !Host
  , bfeBlock      :: !Hash
  , bfeBlockPrev  :: !Hash
  , bfeBlockNo    :: !BlockNo
  , bfeSlotNo     :: !SlotNo
  , bfeSlotStart  :: !UTCTime
  , bfeForged     :: !(Maybe NominalDiffTime)
  , bfeAdopted    :: !(Maybe NominalDiffTime)
  , bfeChainDelta :: !Int
  , bfeAnnounced  :: !(Maybe NominalDiffTime)
  , bfeSending    :: !(Maybe NominalDiffTime)
  }
  deriving (Generic, AE.FromJSON, AE.ToJSON, Show)

bfePrevBlock :: BlockForgerEvents -> Maybe Hash
bfePrevBlock x = case bfeBlockNo x of
  0 -> Nothing
  _ -> Just $ bfeBlockPrev x

-- | Block's events, as seen by an observer.
data BlockObserverEvents
  =  BlockObserverEvents
  { boeHost       :: !Host
  , boeBlock      :: !Hash
  , boeBlockNo    :: !BlockNo
  , boeSlotNo     :: !SlotNo
  , boeSlotStart  :: !UTCTime
  , boeNoticed    :: !(Maybe NominalDiffTime)
  , boeFetched    :: !(Maybe NominalDiffTime)
  , boeAdopted    :: !(Maybe NominalDiffTime)
  , boeChainDelta :: !Int
  , boeAnnounced  :: !(Maybe NominalDiffTime)
  , boeSending    :: !(Maybe NominalDiffTime)
  }
  deriving (Generic, AE.FromJSON, AE.ToJSON, Show)

-- | Sum of observer and forger events alike.
data MachBlockEvents
  = MBFE BlockForgerEvents
  | MBOE BlockObserverEvents
  | MBE  BPError

mbeForgP, mbeObsvP, mbeErrP :: MachBlockEvents -> Bool
mbeForgP = \case
  MBFE{} -> True
  _ -> False
mbeObsvP = \case
  MBOE{} -> True
  _ -> False
mbeErrP = \case
  MBE{} -> True
  _ -> False

mapMbe ::
     (BlockForgerEvents -> a) -> (BlockObserverEvents -> a) -> (BPError -> a)
  -> MachBlockEvents -> a
mapMbe f o e = \case
  MBFE x -> f x
  MBOE x -> o x
  MBE  x -> e x

partitionMbes :: [MachBlockEvents] -> ([BlockForgerEvents], [BlockObserverEvents], [BPError])
partitionMbes = go [] [] []
  where
    go :: [BlockForgerEvents] -> [BlockObserverEvents] -> [BPError] -> [MachBlockEvents] -> ([BlockForgerEvents], [BlockObserverEvents], [BPError])
    go as bs cs [] = (reverse as, reverse bs, reverse cs)
    go as bs cs (MBFE a:xs) = go (a:as) bs cs xs
    go as bs cs (MBOE b:xs) = go as (b:bs) cs xs
    go as bs cs (MBE  c:xs) = go as bs (c:cs) xs

errorMbes :: [MachBlockEvents] -> [BPError]
errorMbes = go []
  where
    go :: [BPError] -> [MachBlockEvents] -> [BPError]
    go cs [] = reverse cs
    go cs (MBE c:xs) = go (c:cs) xs
    go cs (_:xs)      = go    cs  xs

trimapMbe ::
     (BlockForgerEvents -> BlockForgerEvents)
  -> (BlockObserverEvents -> BlockObserverEvents)
  -> (BPError -> BPError)
  -> MachBlockEvents -> MachBlockEvents
trimapMbe f o e = mapMbe (MBFE . f) (MBOE . o) (MBE . e)

bimapMbe ::
     (BlockForgerEvents -> BlockForgerEvents)
  -> (BlockObserverEvents -> BlockObserverEvents)
  -> MachBlockEvents -> MachBlockEvents
bimapMbe f o = trimapMbe f o id

bimapMbe' ::
     (BlockForgerEvents -> Either BPError BlockForgerEvents)
  -> (BlockObserverEvents -> Either BPError BlockObserverEvents)
  -> MachBlockEvents -> MachBlockEvents
bimapMbe' f o = \case
  MBFE x -> either MBE MBFE (f x)
  MBOE x -> either MBE MBOE (o x)
  x@MBE{} -> x

ordBlockEv :: MachBlockEvents -> MachBlockEvents -> Ordering
ordBlockEv l r =
  if      (on (>) $ mapMbe bfeBlockNo boeBlockNo (const 0)) l r then GT
  else if (on (>) $ mapMbe bfeBlockNo boeBlockNo (const 0)) r l then LT
  else if mbeForgP l then GT
  else if mbeForgP r then LT
  else if mbeObsvP l then GT
  else if mbeObsvP r then LT
  else EQ

mbeNoticed, mbeAcquired, mbeAdopted, mbeAnnounced, mbeSending :: MachBlockEvents -> Maybe NominalDiffTime
mbeNoticed   = mapMbe (const $ Just 0) boeNoticed   (const Nothing)
mbeAcquired  = mapMbe bfeForged        boeFetched   (const Nothing)
mbeAdopted   = mapMbe bfeAdopted       boeAdopted   (const Nothing)
mbeAnnounced = mapMbe bfeAnnounced     boeAnnounced (const Nothing)
mbeSending   = mapMbe bfeSending       boeSending   (const Nothing)

mbeBlock :: MachBlockEvents -> Hash
mbeBlock = mapMbe bfeBlock boeBlock eBlock

mbeError :: MachBlockEvents -> Maybe BPError
mbeError = mapMbe (const Nothing) (const Nothing) Just

mbeFailed :: MachBlockEvents -> Bool
mbeFailed = isJust . mbeError

-- | Machine's private view of all the blocks.
type MachBlockMap
  =  Map.Map Hash MachBlockEvents

blockMapHost :: MachBlockMap -> Host
blockMapHost = mapMbe bfeHost boeHost (loHost . eLO) . head . Map.elems

blockMapMaxBlock :: MachBlockMap -> MachBlockEvents
blockMapMaxBlock = maximumBy ordBlockEv . Map.elems

blockMapBlock :: Hash -> MachBlockMap -> MachBlockEvents
blockMapBlock h =
  fromMaybe (error $ "Invariant failed:  missing hash " <> show h) . Map.lookup h

-- | A completed, compactified version of BlockObserverEvents.
data BlockObservation
  =  BlockObservation
  { boObserver   :: !Host
  , boSlotStart  :: !UTCTime
  , boNoticed    :: !NominalDiffTime
  , boFetched    :: !NominalDiffTime
  , boAdopted    :: !(Maybe NominalDiffTime)
  , boChainDelta :: !Int -- ^ ChainDelta during adoption
  , boAnnounced  :: !(Maybe NominalDiffTime)
  , boSending    :: !(Maybe NominalDiffTime)
  }
  deriving (Generic, AE.FromJSON, AE.ToJSON, Show)

-- | All events related to a block.
data BlockEvents
  =  BlockEvents
  { beForger       :: !Host
  , beBlock        :: !Hash
  , beBlockPrev    :: !Hash
  , beBlockNo      :: !BlockNo
  , beSlotNo       :: !SlotNo
  , beSlotStart    :: !UTCTime
  , beForged       :: !NominalDiffTime
  , beAdopted      :: !NominalDiffTime
  , beChainDelta   :: !Int -- ^ ChainDelta during adoption
  , beAnnounced    :: !NominalDiffTime
  , beSending      :: !NominalDiffTime
  , beObservations :: [BlockObservation]
  , beErrors       :: [BPError]
  }
  deriving (Generic, AE.FromJSON, AE.ToJSON, Show)

instance RenderTimeline BlockEvents where
  rtFields =
    --  Width LeftPad
    [ Field 5 0 "block"     "block" "no."     $ IWord64 (unBlockNo . beBlockNo)
    , Field 5 0 "abs.slot"  "abs."  "slot#"   $ IWord64 (unSlotNo . beSlotNo)
    , Field 5 0 "errors"    "error" "count"   $ IInt    (length . beErrors)
    ]
   where
     f w = nChunksEachOf 4 (w + 1) "Block forging events"
     p w = nChunksEachOf 4 (w + 1) "Non-forging peer observation events"

mapChainToForgerEventCDF ::
     [PercSpec Float]
  -> [BlockEvents]
  -> (BlockEvents -> Maybe NominalDiffTime)
  -> Distribution Float NominalDiffTime
mapChainToForgerEventCDF percs cbe proj =
  computeDistribution percs (mapMaybe proj cbe)

mapChainToPeerBlockObservationCDFs ::
     [PercSpec Float]
  -> [BlockEvents]
  -> (BlockObservation -> Maybe NominalDiffTime)
  -> String
  -> (Distribution Float NominalDiffTime, Distribution Float NominalDiffTime)
mapChainToPeerBlockObservationCDFs percs cbe proj desc =
  (means, covs)
 where
   means, covs :: Distribution Float NominalDiffTime
   (,) means covs = computeDistributionStats desc
                      (fmap realToFrac <$> allDistributions)
                    & either error id
                    & join (***) (fmap realToFrac)

   allDistributions :: [Distribution Float NominalDiffTime]
   allDistributions = computeDistribution percs <$> allObservations

   allObservations :: [[NominalDiffTime]]
   allObservations = blockObservations <$> cbe

   blockObservations :: BlockEvents -> [NominalDiffTime]
   blockObservations be = mapMaybe proj (beObservations be)

blockProp :: ChainInfo -> [(JsonLogfile, [LogObject])] -> IO BlockPropagation
blockProp ci xs = do
  putStrLn ("blockProp: recovering block event maps" :: String)
  doBlockProp =<< mapConcurrently (pure . blockEventsFromLogObjects ci) xs

doBlockProp :: [MachBlockMap] -> IO BlockPropagation
doBlockProp eventMaps = do
  putStrLn ("tip block: " <> show tipBlock :: String)
  putStrLn ("chain length: " <> show (length chain) :: String)
  pure $ BlockPropagation
    (forgerEventsCDF    (Just . beForged))
    (forgerEventsCDF    (\x -> if beChainDelta x == 1 then Just (beAdopted x)
                               else Nothing))
    (forgerEventsCDF    (Just . beAnnounced))
    (forgerEventsCDF    (Just . beSending))
    (observerEventsCDFs (Just . boNoticed) "peer noticed")
    (observerEventsCDFs (Just . boFetched) "peer fetched")
    (observerEventsCDFs (\x -> if boChainDelta x == 1 then boAdopted x
                               else Nothing) "peer adopted")
    (observerEventsCDFs boAnnounced "peer announced")
    (observerEventsCDFs boSending   "peer sending")
    chain
 where
   forgerEventsCDF    = mapChainToForgerEventCDF           stdPercentiles chain
   observerEventsCDFs = mapChainToPeerBlockObservationCDFs stdPercentiles chain

   chain          = rebuildChain eventMaps tipHash
   tipBlock       = getBlockForge eventMaps tipHash
   tipHash        = rewindChain eventMaps 1 (mbeBlock finalBlockEv)
   finalBlockEv   = maximumBy ordBlockEv $ blockMapMaxBlock <$> eventMaps

   rewindChain :: [MachBlockMap] -> Int -> Hash -> Hash
   rewindChain eventMaps count tip = go tip count
    where go tip = \case
            0 -> tip
            n -> go (bfeBlockPrev $ getBlockForge eventMaps tip) (n - 1)

   getBlockForge :: [MachBlockMap] -> Hash -> BlockForgerEvents
   getBlockForge xs h =
     mapMaybe (Map.lookup h) xs
     & find mbeForgP
     & fromMaybe
        (error $ mconcat
         [ "Invariant failed: couldn't find a forge for hash ", show h
         , "\nErrors:\n", show (intercalate "\n" $ fmap show $ errorMbes $ mapMaybe (Map.lookup h) xs)
         ])
     & mapMbe id (error "Silly invariant failed.") (error "Silly invariant failed.")

   rebuildChain :: [MachBlockMap] -> Hash -> [BlockEvents]
   rebuildChain machBlockMaps tip = go (Just tip) []
    where go Nothing  acc = acc
          go (Just h) acc =
            case partitionMbes $ mapMaybe (Map.lookup h) machBlockMaps of
              ([], _, ers) -> error $ mconcat
                [ "No forger for hash ", show h
                , "\nErrors:\n"
                ] ++ intercalate "\n" (show <$> ers)
              blkEvs@(forgerEv:_, oEvs, ers) ->
                go (bfePrevBlock forgerEv) (liftBlockEvents forgerEv oEvs ers : acc)

   liftBlockEvents :: BlockForgerEvents -> [BlockObserverEvents] -> [BPError] -> BlockEvents
   liftBlockEvents BlockForgerEvents{..} os errs =
     BlockEvents
     { beForger     = bfeHost
     , beBlock      = bfeBlock
     , beBlockPrev  = bfeBlockPrev
     , beBlockNo    = bfeBlockNo
     , beSlotNo     = bfeSlotNo
     , beSlotStart  = bfeSlotStart
     , beForged     = bfeForged    & miss "Forged"
     , beAdopted    = bfeAdopted   & miss "Adopted (forger)"
     , beChainDelta = bfeChainDelta
     , beAnnounced  = bfeAnnounced & miss "Announced (forger)"
     , beSending    = bfeSending   & miss "Sending (forger)"
     , beObservations = catMaybes $
       os <&> \BlockObserverEvents{..}->
         BlockObservation
           <$> Just boeHost
           <*> Just bfeSlotStart
           <*> boeNoticed
           <*> boeFetched
           <*> Just boeAdopted
           <*> Just boeChainDelta
           <*> Just boeAnnounced
           <*> Just boeSending
     , beErrors = errs
     }
    where
      miss :: String -> Maybe a -> a
      miss slotDesc = fromMaybe $ error $ mconcat
       [ "While processing ", show bfeBlockNo, " hash ", show bfeBlock
       , " forged by ", show bfeHost
       , " -- missing slot: ", slotDesc
       ]

-- | Given a single machine's log object stream, recover its block map.
blockEventsFromLogObjects :: ChainInfo -> (JsonLogfile, [LogObject]) -> MachBlockMap
blockEventsFromLogObjects ci (fp, xs) =
  foldl (blockPropMachEventsStep ci fp) mempty xs

blockPropMachEventsStep :: ChainInfo -> JsonLogfile -> MachBlockMap -> LogObject -> MachBlockMap
blockPropMachEventsStep ci (JsonLogfile fp) bMap lo = case lo of
  LogObject{loAt, loHost, loBody=LOBlockForged{loBlock,loPrev,loBlockNo,loSlotNo}} ->
    mbmGetForger lo loBlock bMap "LOBlockForged"
    & fromMaybe
      (MBFE $ BlockForgerEvents
        loHost loBlock loPrev loBlockNo loSlotNo (slotStart ci loSlotNo)
        (Just $ loAt `diffUTCTime` slotStart ci loSlotNo) Nothing 0 Nothing Nothing)
    & doInsert loBlock
  LogObject{loAt, loHost, loBody=LOChainSyncClientSeenHeader{loBlock,loBlockNo,loSlotNo}} ->
    let mbe0 = Map.lookup loBlock bMap
    in if isJust mbe0 then bMap else
      (MBOE $
       BlockObserverEvents
         loHost loBlock loBlockNo loSlotNo (slotStart ci loSlotNo)
         (Just $ loAt `diffUTCTime` slotStart ci loSlotNo) Nothing Nothing 0 Nothing Nothing)
      & doInsert loBlock
  LogObject{loAt, loHost, loBody=LOBlockFetchClientCompletedFetch{loBlock}} ->
    let mbe0 = mbmGetObserver lo loBlock bMap "LOBlockFetchClientCompletedFetch"
    in
      deltaTS "LOBlockFetchClientCompletedFetch"
        loAt mbe0 [mbeNoticed]
      <&> (\v ->
           bimapMbe'
           (const . Left $ fail' loBlock "LOBlockFetchClientCompletedFetch after BFE")
           (\x -> Right x { boeFetched=Just v })
           mbe0)
      & doInsert loBlock . join either id
  LogObject{loAt, loHost, loBody=LOBlockAddedToCurrentChain{loBlock,loChainLengthDelta}} ->
    let mbe0 = Map.lookup loBlock bMap
               & fromMaybe (fail loBlock "LOBlockAddedToCurrentChain")
    in if isJust (mbeAdopted mbe0) then bMap else
      deltaTS "LOBlockAddedToCurrentChain"
        loAt mbe0 [mbeNoticed, mbeAcquired]
      <&> (\v ->
          bimapMbe
          (\x -> x { bfeAdopted=Just v, bfeChainDelta=loChainLengthDelta })
          (\x -> x { boeAdopted=Just v, boeChainDelta=loChainLengthDelta })
          mbe0)
      & doInsert loBlock . join either id
  LogObject{loAt, loHost, loBody=LOChainSyncServerSendHeader{loBlock}} ->
    let mbe0 = Map.lookup loBlock bMap
               & fromMaybe (fail loBlock "LOChainSyncServerSendHeader")
    in if isJust (mbeAnnounced mbe0) then bMap else
      -- bimapMbe
      -- (\x -> x { bfeAnnounced=Just $
      --            fromMaybe (error "")
      --            deltaT loAt mbe0 [Right mbeNoticed, Right mbeAcquired, Left mbeAdopted]})
      -- (\x -> x { boeAnnounced=Just $ deltaT loAt mbe0 [Right mbeNoticed, Right mbeAcquired, Left mbeAdopted]})
      -- mbe0
      deltaT "LOChainSyncServerSendHeader"
        loAt mbe0 [Right mbeNoticed, Right mbeAcquired, Left mbeAdopted]
      <&> (\v ->
          bimapMbe
          (\x -> x { bfeAnnounced=Just v })
          (\x -> x { boeAnnounced=Just v })
          mbe0)
      & doInsert loBlock . join either id
  LogObject{loAt, loHost, loBody=LOBlockFetchServerSending{loBlock}} ->
    let mbe0 = Map.lookup loBlock bMap
               & fromMaybe (fail loBlock "LOBlockFetchServerSending leads absolutely")
    in if isJust (mbeSending mbe0) then bMap else
      deltaT "LOBlockFetchServerSending"
        loAt mbe0 [Right mbeNoticed, Right mbeAcquired, Left mbeAdopted, Left mbeAnnounced]
      <&> (\v ->
           bimapMbe
           (\x -> x { bfeSending=Just v })
           (\x -> x { boeSending=Just v })
           mbe0)
      & doInsert loBlock . join either id
  _ -> bMap
 where
   fail' :: Hash -> String -> BPError
   fail' hash desc = BPError lo hash (Just fp) desc

   fail :: Hash -> String -> MachBlockEvents
   fail hash desc = MBE $ fail' hash desc

   doInsert :: Hash -> MachBlockEvents -> MachBlockMap
   doInsert k x = Map.insert k x bMap

   deltaTS :: String -> UTCTime -> MachBlockEvents -> [MachBlockEvents -> Maybe NominalDiffTime] -> Either MachBlockEvents NominalDiffTime
   deltaTS desc t mbe = deltaT desc t mbe . fmap Right

   deltaT :: String -> UTCTime -> MachBlockEvents -> [Either (MachBlockEvents -> Maybe NominalDiffTime) (MachBlockEvents -> Maybe NominalDiffTime)] -> Either MachBlockEvents NominalDiffTime
   deltaT desc t mbe mdtProjs =
     (t `diffUTCTime`) <$>
       foldM (\tv emdt ->
                either
                  (Right . fromMaybe (-0.01))
                  (maybe (Left $ fail (mbeBlock mbe) (desc <> " leads")) Right)
                  emdt
                <&> flip addUTCTime tv)
             (mapMbe bfeSlotStart boeSlotStart (const zeroUTCTime) mbe)
             (join bimap ($ mbe) <$> mdtProjs)

   mbmGetForger :: LogObject -> Hash -> MachBlockMap -> String -> Maybe MachBlockEvents
   mbmGetForger lo ha m eDesc = Map.lookup ha m <&>
     bimapMbe' Right
               (const.Left $ BPError lo ha (Just fp) (eDesc <> " after a BlockObserverEvents"))

   mbmGetObserver :: LogObject -> Hash -> MachBlockMap -> String -> MachBlockEvents
   mbmGetObserver lo ha m eDesc = Map.lookup ha m <&>
     bimapMbe' (const.Left $ BPError lo ha (Just fp) (eDesc <> " after a BlockForgerEvents"))
               Right
     & fromMaybe (MBE $ BPError lo ha (Just fp) (eDesc <> " as first block event"))
