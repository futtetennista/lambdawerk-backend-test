{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
module Person ( Person(..)
              , parseFile
              )
where

import Lib.Prelude
import qualified Text.XML.Stream.Parse as XML
import Data.XML.Types (Event)
import Conduit
import Data.Maybe (fromMaybe)
import Data.Vector (Vector)
import qualified Data.Aeson as JSON
import GHC.Generics (Generic)


data Person =
  Person { lname :: Text
         , fname :: Text
         , bos :: Iso8601Date
         , phone :: Text
         }
  deriving (Show, Generic)


-- Automatic (and fast!) decoding to corresponing JSON object
instance JSON.ToJSON Person where
  toEncoding =
    JSON.genericToEncoding JSON.defaultOptions


type Iso8601Date =
  Text


parsePerson :: MonadThrow m => Consumer Event m (Maybe Person)
parsePerson =
  XML.tagNoAttr "member" $
    Person <$> tagContentOrEmpty "firstname"
           <*> tagContentOrEmpty "lastname"
           <*> tagContentOrEmpty "date-of-birth"
           <*> tagContentOrEmpty "phone"
    where
      tagContentOrEmpty tagName =
        XML.tagNoAttr tagName XML.content >>= contentOrEmptyText

      contentOrEmptyText =
        return . fromMaybe ""


parsePeople :: MonadThrow m => Conduit Event m Person
parsePeople =
  void $ XML.tagNoAttr "members" $ XML.manyYield parsePerson


parseFile :: Int -> FilePath -> Producer (ResourceT IO) (Vector Person)
parseFile batchSize fp =
  XML.parseFile XML.def fp
    .| parsePeople
    .| conduitVector batchSize
