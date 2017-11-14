module Database ( UpsertionResult
                , Config(..)
                , upsert
                )
where

import Lib.Prelude hiding (handle)
import Person (Person(..))
import Data.Vector (Vector)
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import Control.Exception.Safe
import Network.URL
import Data.ByteString.Char8 (unpack)


upsert :: (MonadCatch m, MonadIO m) => Config -> Vector Person -> m UpsertionResult
upsert config persons =
  handle mkUpsertionFailure $ do
    response <- httpLbs =<< mkRequest
    if statusIsSuccessful (getResponseStatus response)
      then return $ Right ()
      else return $ Left GeneralException
  where
    mkRequest :: MonadThrow m => m Request
    mkRequest =
      case murl of
        Nothing ->
          throw InvalidEndpointURLException

        Just url -> do
          initReq <- parseRequest (exportURL url)
          return $ setRequestMethod "POST"
                 $ setRequestBodyJSON persons
                 $ addRequestHeader "Authorization" ("Bearer: " <> configJWTToken config)
                 $ initReq


    murl :: Maybe URL
    murl =
      importURL . unpack $ configDBEndpointURL config <> "/public/rpc/upsert"


mkUpsertionFailure :: (MonadCatch m, MonadIO m) => SomeException -> m UpsertionResult
mkUpsertionFailure exception =
  case fromException exception of
    Just (HttpExceptionRequest _ _) ->
      return $ Left ConnectionException

    _ ->
      return $ Left GeneralException


type UpsertionResult =
  Either UpsertionException ()


data UpsertionException
  = GeneralException
  | ConnectionException
  | InvalidEndpointURLException
  deriving (Show, Typeable)


instance Exception UpsertionException


data Config =
  Config { configDBEndpointURL :: ByteString
         , configJWTToken :: ByteString
         }
  deriving Show
