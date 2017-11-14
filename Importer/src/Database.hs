{-# LANGUAGE DeriveGeneric #-}
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
import qualified Data.Vector as V


upsert :: (MonadCatch m, MonadIO m) => Config -> Vector Person -> m UpsertionResult
upsert config ps =
  handle mkUpsertionFailure $ do
    response <- httpLbs =<< mkRequest
    if statusIsSuccessful (getResponseStatus response)
      then return $ Right (V.length ps)
      else return $ Left GeneralException
  where
    mkRequest :: MonadThrow m => m Request
    mkRequest =
      case murl of
        Nothing ->
          throw InvalidEndpointURLException

        Just url ->
          return . setRequestMethod "POST"
                 . setRequestBodyJSON (UpsertRequestBody ps)
                 . addRequestHeader "User-Agent" "importer/0.0.1"
                 . addRequestHeader "Authorization" ("Bearer " <> configJWT config)
                 =<< parseRequest (exportURL url)


    murl :: Maybe URL
    murl =
      importURL . unpack $ configDBEndpointURL config <> "/rpc/upsert"


mkUpsertionFailure :: (MonadCatch m, MonadIO m) => SomeException -> m UpsertionResult
mkUpsertionFailure exception =
  case fromException exception of
    Just e@(HttpExceptionRequest _ _) ->
      do print e ; return $ Left ConnectionException

    e@_ ->
      do print e ; return $ Left GeneralException


type UpsertionResult =
  Either UpsertionException Int


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


data UpsertRequestBody =
  UpsertRequestBody { members :: Vector Person }
  deriving (Generic, Show)


-- Automatic (and fast!) decoding to corresponing JSON object
instance JSON.ToJSON UpsertRequestBody where
  toEncoding =
    JSON.genericToEncoding JSON.defaultOptions
