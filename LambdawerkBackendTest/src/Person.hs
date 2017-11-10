{-# LANGUAGE DeriveGeneric #-}
module Person ( Person(..)
              , parseFile
              )
where

import Lib.Prelude
import qualified Text.XML.Stream.Parse as XML
import Data.XML.Types (Event)
import Conduit
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Data.Aeson as JSON
import GHC.Generics (Generic)


data Person =
  Person { lname :: T.Text
         , fname :: T.Text
         , bos :: Iso8601Date
         , phone :: T.Text
         }
  deriving (Show, Generic)


-- Automatic (and fast!) decoding to corresponing JSON object
instance JSON.ToJSON Person where
  toEncoding =
    JSON.genericToEncoding JSON.defaultOptions


type Iso8601Date =
  T.Text


parsePerson :: MonadThrow m => Consumer Event m (Maybe Person)
parsePerson =
  XML.tagNoAttr "member" $
    Person <$> tagContentOrEmpty "firstname"
           <*> tagContentOrEmpty "lastname"
           <*> tagContentOrEmpty "date-of-birth"
           <*> tagContentOrEmpty "phone"
    where
      tagContentOrEmpty tagName =
        XML.tagNoAttr tagName XML.content >>= return . fromMaybe ""


parsePeople :: MonadThrow m => Conduit Event m Person
parsePeople =
  void $ XML.tagNoAttr "members" $ XML.manyYield parsePerson


parseFile :: Int -> FilePath -> Conduit Person (ResourceT IO) (V.Vector Person)
parseFile batchSize fp =
  -- runConduitRes $
  XML.parseFile XML.def fp
    .| parsePeople
    .| conduitVector batchSize
