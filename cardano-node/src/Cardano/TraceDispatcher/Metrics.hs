module Cardano.TraceDispatcher.Metrics
  (
    startResourceTracer
  , docResourceStats
  ) where


import           Cardano.Logging
import           Cardano.Prelude hiding (trace)

startResourceTracer ::
     Trace IO ResourceStats
  -> IO ()
startResourceTracer tr = do
    void $ forkIO $ forever $ do
      mbrs <- readResourceStats
      case mbrs of
        Just rs -> traceWith tr rs
        Nothing -> pure ()
      threadDelay 1000000 -- TODO JNF:  make configurable
                               -- in microseconds

docResourceStats :: Documented (ResourceStats)
docResourceStats = Documented [
      DocMsg
        (ResourceStats 1 1 1 1 1 1 1 1 1 1)
        []
        "TODO jnf."
    ]
