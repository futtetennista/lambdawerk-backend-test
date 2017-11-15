module Stats ( Stats(..)
             , mkStats
             , prettyPrint
             )
where

import Lib.Prelude
import Person (Person)
import Database (UpsertionResult)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime, diffTimeToPicoseconds)


data Stats =
  Stats { duration :: Int
        , successes :: Int
        , failures :: [Person]
        , insertions :: Int
        , updates :: Int
        }


mkStats :: (UTCTime, UTCTime) -> [UpsertionResult] -> Stats
mkStats =
  undefined


prettyPrint :: Stats -> Text
prettyPrint =
  undefined
