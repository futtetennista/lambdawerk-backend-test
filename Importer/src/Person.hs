{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
module Person ( Person(..)
              , parseInputFile
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
import qualified Data.Text as T
import Data.Time.Format (parseTimeM, defaultTimeLocale, iso8601DateFormat)
import Data.Time.Clock (UTCTime)
import GHC.Base (String)


{-
MEMORY COST (in Words aka 8 bytes on 64bits machines):
  1 (header)
+ 1 + #fname
+ 1 + #lname
+ 1 + 10
+ 1 + 10

where #field is the length of the text field which varies with the input
-}
data Person =
  Person { lname :: Text
         , fname :: Text
         , dob :: Iso8601Date
         , phone :: Text
         }
  deriving (Eq, Show, Generic)


-- Automatic (and fast!) decoding to corresponding JSON object
instance JSON.ToJSON Person where
  toEncoding =
    JSON.genericToEncoding JSON.defaultOptions


type Iso8601Date =
  Text


-- builds a valid person or fail
mkPerson :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Person
mkPerson mfname mlname mdob mphone =
  Person (checkField "lname" mfname)
    (checkField "fname" mlname)
    (checkField "dob" mdob)
    (checkField "phone" mphone)
  where
    checkField :: Text -> Maybe Text -> Text
    checkField "dob" =
      checkIso8601Date . fromMaybe ""
    checkField _ =
      fromMaybe ""


checkIso8601Date :: Text -> Iso8601Date
checkIso8601Date xs =
  maybe "-infinity" (const xs) $ parseIso8601Date xs
  where
    parseIso8601Date :: Text -> Maybe UTCTime
    parseIso8601Date =
      parseTimeM False defaultTimeLocale iso8601DateFormatNoTime . T.unpack


iso8601DateFormatNoTime :: String
iso8601DateFormatNoTime =
  iso8601DateFormat Nothing


parsePerson :: (MonadIO m, MonadThrow m) => Consumer Event m (Maybe Person)
parsePerson =
  XML.tagNoAttr "member" $
    mkPerson <$> content "firstname"
      <*> content "lastname"
      <*> content "date-of-birth"
      <*> content "phone"
  where
    content tag =
      XML.tagNoAttr tag XML.content


parsePeople :: (MonadIO m, MonadThrow m) => Conduit Event m Person
parsePeople =
  void $ XML.tagNoAttr "members" $ XML.manyYield parsePerson


parseInputFile :: Int -> FilePath -> Producer (ResourceT IO) (Vector Person)
parseInputFile batchSize fp =
  XML.parseFile XML.def fp
    .| parsePeople
    .| conduitVector batchSize
