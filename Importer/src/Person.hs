{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE QuasiQuotes #-}
module Person ( Person(..)
              , parseXMLInputFile
              )
where

import Lib.Prelude
import qualified Text.XML.Stream.Parse as XML
-- import qualified Text.XML.Stream.Render as XMLRender
-- import Text.Hamlet.XML (xml)
import Data.XML.Types (Event(..))
import Conduit
import Data.Maybe (fromMaybe)
import Data.Vector (Vector)
import qualified Data.Aeson as JSON
import GHC.Generics (Generic)
import qualified Data.Text as T
import Data.Time.Format (parseTimeM, defaultTimeLocale, iso8601DateFormat)
import Data.Time.Clock (UTCTime)
import GHC.Base (String)
import Types (BatchSize)


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


parseEntry :: (MonadIO m, MonadThrow m) => Consumer Event m (Maybe Person)
parseEntry =
  XML.tagNoAttr "member" $
    mkPerson <$> content "firstname"
      <*> content "lastname"
      <*> content "date-of-birth"
      <*> content "phone"
  where
    content tag =
      XML.tagNoAttr tag XML.content


parseEntries :: (MonadIO m, MonadThrow m) => Conduit Event m Person
parseEntries =
  void $ XML.tagNoAttr "members" $ XML.manyYield parseEntry


parseXMLInputFile :: BatchSize -> FilePath -> Producer (ResourceT IO) (Vector Person)
parseXMLInputFile batchSize fp =
  XML.parseFile XML.def fp
    .| parseEntries
    .| conduitVector batchSize


-- entry p =
--   [xml|
-- <member>
--   <firstname> fname p
--   <lastname> lname p
--   <date-of-birth> dob p
--   <phone> phone p
-- |]


-- renderFailedEntries :: Conduit Event m XMLRender.Builder
-- renderFailedEntries = undefined
  -- XMLRender.renderBuilder XML.def
    -- .| return EventBeginDocument
    -- .| return EventBeginElement "members"
  -- XML.Document (XML.Prologue [] Nothing []) root []
  -- where
  --   root =
  --     Element "members" empty entries
