module Stats ( Stats(..)
             , mkStats
             , prettyPrint
             )
where

import Lib.Prelude
import Types (ImporterResult)
import Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime)


data Stats =
  Stats { duration :: NominalDiffTime
        , successes :: Int
        , failures :: Int
        , modifications :: Int
        }
  deriving Show


mkStats :: (UTCTime, UTCTime) -> [ImporterResult Int] -> Stats
mkStats (startTime, endTime) results =
  Stats totalTime okCount koCount modifiedRowsCount
  where
    totalTime =
      diffUTCTime endTime startTime

    (koCount, okCount, modifiedRowsCount) =
      foldr accumulateResults (0, 0, 0) results

    accumulateResults :: ImporterResult Int -> (Int, Int, Int) -> (Int, Int, Int)
    accumulateResults ur (kos, oks, ms) =
      either (\memberCount -> (kos + memberCount, oks, ms))
             (\(memberCount, rowCount) -> (kos, oks + memberCount, ms + rowCount))
             ur


prettyPrint :: Stats -> IO ()
prettyPrint stats = do
  print ("import took " <> show (duration stats) :: Text)
  print ("Failures: " <> show (failures stats)
         <> "\tModifications requested:"
         <> show (successes stats)
         <> "\tModifications applied:"
         <> show (modifications stats)
         :: Text)
