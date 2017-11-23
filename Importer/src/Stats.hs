module Stats ( Stats(..)
             , prettyPrint
             , emptyStats
             , updateDuration
             , updateResults
             )
where

import Lib.Prelude
import Types (MergeJobResult)
import Data.Time.Clock (UTCTime(..), NominalDiffTime, diffUTCTime, secondsToDiffTime)
import Data.Time.Calendar (Day(..))


data Stats =
  Stats { duration :: NominalDiffTime
        , successes :: Int
        , failures :: Int
        , modifications :: Int
        }
  deriving Show


emptyStats :: Stats
emptyStats =
  Stats nullDuration 0 0 0
  where
    nullTime =
      UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)

    nullDuration =
      diffUTCTime nullTime nullTime


updateDuration :: UTCTime -> UTCTime -> Stats -> Stats
updateDuration startTime endTime stats =
  stats{ duration = diffUTCTime endTime startTime }


updateResults :: MergeJobResult Int -> Stats -> Stats
updateResults result stats =
  stats{ successes = okCount, failures = koCount, modifications = modifiedRowsCount }
  where
    (koCount, okCount, modifiedRowsCount) =
      case result of
        Left entryCount ->
          (failures stats + entryCount, successes stats, modifications stats)

        Right (entryCount, rowCount) ->
          (failures stats, entryCount + successes stats, rowCount + modifications stats)


prettyPrint :: MonadIO m => Stats -> m ()
prettyPrint stats = do
  print ("import took " <> show (duration stats) :: Text)
  print ("Failures: " <> show (failures stats)
         <> "\tModifications requested:"
         <> show (successes stats)
         <> "\tModifications applied:"
         <> show (modifications stats)
         :: Text)
