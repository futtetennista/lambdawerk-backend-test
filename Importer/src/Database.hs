{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import qualified Data.Aeson as JSON


upsert :: (MonadCatch m, MonadIO m) => Config -> Vector Person -> m UpsertionResult
upsert config xs =
  handle mkUpsertionFailure $ do
    response <- httpLbs =<< mkRequest
    if statusIsSuccessful (getResponseStatus response)
      then return $ Right ()
      else do print ("Unsuccessful" :: Text) ; return $ Left GeneralException
  where
    mkRequest :: MonadThrow m => m Request
    mkRequest =
      case murl of
        Nothing ->
          throw InvalidEndpointURLException

        Just url ->
          return . setRequestMethod "POST"
                 . setRequestBodyJSON (UpsertRequestBody xs)
                 . addRequestHeader "Authorization" ("Bearer: " <> configJWT config)
                 =<< parseRequest (exportURL url)


    murl :: Maybe URL
    murl =
      importURL . unpack $ configDBEndpointURL config <> "/public/rpc/upsert"


mkUpsertionFailure :: (MonadCatch m, MonadIO m) => SomeException -> m UpsertionResult
mkUpsertionFailure exception =
  case fromException exception of
    Just (HttpExceptionRequest _ _) ->
      do print ("Failure1" :: Text) ;  return $ Left ConnectionException

    _ ->
      do print ("Failure2" :: Text) ; return $ Left GeneralException


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
         , configJWT :: ByteString
         }
  deriving Show


newtype UpsertRequestBody =
  UpsertRequestBody { members :: Vector Person }
  deriving (JSON.ToJSON, Show)
