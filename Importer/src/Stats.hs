module Stats ( Stats(..)
             , mkStats
             , prettyPrint
             )
where

import Lib.Prelude
import Person (Person)
import Database (UpsertionResult)
import Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime)


data Stats =
  Stats { duration :: NominalDiffTime
        , successes :: Int
        , failures :: [Person]
        , modifications :: Int
        }
  deriving Show


mkStats :: (UTCTime, UTCTime) -> [UpsertionResult [Person]] -> Stats
mkStats (startTime, endTime) results =
  Stats totalTime oks kos n
  where
    totalTime =
      diffUTCTime endTime startTime

    (kos, oks, n) =
      foldr accumulateResults ([], 0, 0) results

    accumulateResults :: UpsertionResult [Person]
                      -> ([Person], Int, Int)
                      -> ([Person], Int, Int)
    accumulateResults ur (fs, ok, m) =
      either (const (fs, ok, n))
             (\(members, mods) -> (fs, ok + members, m + mods))
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
