module Person ( Person(..)
              , Row
              , parseFile
              , toRow
              )
where

import Lib.Prelude
import qualified Text.XML.Stream.Parse as Parser
import Data.XML.Types (Event)
import Conduit
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V


data Person =
  Person T.Text T.Text Iso8601Date T.Text
  deriving Show


type Iso8601Date =
  T.Text


parsePerson :: MonadThrow m => Consumer Event m (Maybe Person)
parsePerson =
  Parser.tagNoAttr "member" $
    Person <$> tagContentOrEmpty "firstname"
           <*> tagContentOrEmpty "lastname"
           <*> tagContentOrEmpty "date-of-birth"
           <*> tagContentOrEmpty "phone"
    where
      tagContentOrEmpty tagName =
        Parser.tagNoAttr tagName Parser.content >>= return . fromMaybe ""


parsePeople :: MonadThrow m => Conduit Event m Person
parsePeople =
  void $ Parser.tagNoAttr "members" $ Parser.manyYield parsePerson
