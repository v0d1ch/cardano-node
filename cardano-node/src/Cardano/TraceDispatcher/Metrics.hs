module Cardano.TraceDispatcher.Metrics
  (
    startResourceTracer
  , docResourceStats
  ) where


import           Data.Word
import qualified GHC.Stats as GhcStats
import           Data.Aeson (Value (Number, String), (.=))
import           System.CPUTime

import           Cardano.Logging
import           Cardano.Prelude hiding (trace)


-- | Struct for resources used by the process
data ResourceStats
  = ResourceStats
      { rCentiCpu   :: !Word64
      , rCentiGC    :: !Word64
      , rCentiMut   :: !Word64
      , rGcsMajor   :: !Word64
      , rGcsMinor   :: !Word64
      , rAlloc      :: !Word64
      , rLive       :: !Word64
      , rRSS        :: !Word64
      , rCentiBlkIO :: !Word64
      , rThreads    :: !Word64
      }
  deriving (Show)

startResourceTracer ::
     Trace IO ResourceStats
  -> IO ()
startResourceTracer tr = do
    void $ forkIO $ forever $ do
      rs <- readResourceStats
      traceWith tr rs
      threadDelay 1000000 -- TODO JNF:  make configurable
                               -- in microseconds

instance LogFormatting ResourceStats where
    forHuman rs = "Resources: CpuTicks " <> show (rCentiCpu rs)
                  <> ", Resident " <> show (rRSS rs)
                  <> ", GcLiveBytes " <> show (rLive rs)
                  <> ", GcMajorNum " <> show (rGcsMajor rs)
                  <> ", GcMinorNum " <> show (rGcsMinor rs)
                  <> ", Gcticks " <> show (rCentiGC rs)
                  <> ", Mutticks " <> show (rCentiMut rs)
                  <> ", Threads " <> show (rThreads rs)
                  <> "."

    forMachine _dtal rs = mkObject
      [ "kind"          .= String "ResourceStats"
      , "Cputicks"      .= Number (fromIntegral $ rCentiCpu rs)
      , "Resident"      .= Number (fromIntegral $ rRSS rs)
      , "GcLiveBytes"   .= Number (fromIntegral $ rLive rs)
      , "GcMajorNum"    .= Number (fromIntegral $ rGcsMajor rs)
      , "GcMinorNum"    .= Number (fromIntegral $ rGcsMinor rs)
      , "Gcticks"       .= Number (fromIntegral $ rCentiGC rs)
      , "Mutticks"      .= Number (fromIntegral $ rCentiMut rs)
      , "Threads"       .= Number (fromIntegral $ rThreads rs)
      ]

    asMetrics rs = [
        IntM ["Stat","Cputicks"] (fromIntegral $ rCentiCpu rs)
      , IntM ["Mem","Resident"] (fromIntegral $ rRSS rs)
      , IntM ["RTS","GcLiveBytes"] (fromIntegral $ rLive rs)
      , IntM ["RTS","GcMajorNum"] (fromIntegral $ rGcsMajor rs)
      , IntM ["RTS","GcMinorNum"] (fromIntegral $ rGcsMinor rs)
      , IntM ["RTS","Gcticks"] (fromIntegral $ rCentiGC rs)
      , IntM ["RTS","Mutticks"] (fromIntegral $ rCentiMut rs)
      , IntM ["Stat","Threads"] (fromIntegral $ rThreads rs)
      ]

readResourceStats :: IO ResourceStats
readResourceStats = do
  cpu <- getCPUTime
  rts <- GhcStats.getRTSStats
  pure $
    ResourceStats
    { rCentiCpu   = intToCenti cpu
    , rCentiGC    = nsToCenti $ GhcStats.gc_cpu_ns rts
    , rCentiMut   = nsToCenti $ GhcStats.mutator_cpu_ns rts
    , rGcsMajor   = fromIntegral $ GhcStats.major_gcs rts
    , rGcsMinor   = fromIntegral $ GhcStats.gcs rts - GhcStats.major_gcs rts
    , rAlloc      = GhcStats.allocated_bytes rts
    , rLive       = GhcStats.gcdetails_live_bytes $ GhcStats.gc rts
    , rRSS        = 0
    , rCentiBlkIO = 0
    , rThreads    = 0
    }
 where
   nsToCenti :: GhcStats.RtsTime -> Word64
   nsToCenti = fromIntegral . (`div` 10000000)
   intToCenti :: Integer -> Word64
   intToCenti = fromIntegral . (`div` 10000000)

docResourceStats :: Documented (ResourceStats)
docResourceStats = Documented [
      DocMsg
        (ResourceStats 1 1 1 1 1 1 1 1 1 1)
        []
        "TODO jnf."
    ]
