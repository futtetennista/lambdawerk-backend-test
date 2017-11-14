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


data Person =
  Person { lname :: Text
         , fname :: Text
         , dob :: Iso8601Date
         , phone :: Text
         }
  deriving (Show, Generic)


-- Automatic (and fast!) decoding to corresponing JSON object
instance JSON.ToJSON Person where
  toEncoding =
    JSON.genericToEncoding JSON.defaultOptions


type Iso8601Date =
  Text


-- builds a valid person or fail
mkPerson :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Person
mkPerson mfn mln md mp =
  Person <$> checkField "lname" mfn
    <*> checkField "fname" mln
    <*> checkField "dob" md
    <*> checkField "phone" mp
  where
    checkField :: Text -> Maybe Text -> Maybe Text
    checkField "phone" =
      (\n -> if T.length n == 10 then Just n else Nothing) . fromMaybe ""
    checkField "fname" =
      checkEmpty . fromMaybe ""
    checkField "lname" =
      checkEmpty . fromMaybe ""
    checkField "dob" =
      validateIso8601Date . fromMaybe ""
    checkField _ =
      return . fromMaybe ""

    checkEmpty xs =
      if T.null xs then Nothing else Just xs


validateIso8601Date :: Text -> Maybe Iso8601Date
validateIso8601Date xs =
  const xs `fmap` parseIso8601Date xs
  where
    parseIso8601Date :: Text -> Maybe UTCTime
    parseIso8601Date =
      parseTimeM False defaultTimeLocale (iso8601DateFormat Nothing) . T.unpack


parsePerson :: MonadThrow m => Consumer Event m (Maybe Person)
parsePerson =
  --- this is needed only to "squash" the two `Maybe`s and return the appropriate type
  join `fmap` consumerMmperson
    where
      consumerMmperson :: MonadThrow m => Consumer Event m (Maybe (Maybe Person))
      consumerMmperson =
        XML.tagNoAttr "member" $
          mkPerson <$> content "firstname"
            <*> content "lastname"
            <*> content "date-of-birth"
            <*> content "phone"

      content tag =
        XML.tagNoAttr tag XML.content


parsePeople :: MonadThrow m => Conduit Event m Person
parsePeople =
  void $ XML.tagNoAttr "members" $ XML.manyYield parsePerson


parseInputFile :: Int -> FilePath -> Producer (ResourceT IO) (Vector Person)
parseInputFile batchSize fp =
  XML.parseFile XML.def fp
    .| parsePeople
    .| conduitVector batchSize
