{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
module Database ( UpsertionResult
                , UpsertionException
                , ModificationCount
                , Config(..)
                , ExceptionHandler
                , upsert
                , upsertionExceptionHandler
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


upsert :: (MonadCatch m, MonadIO m) => Config -> Vector Person -> m (UpsertionResult [Person])
upsert config ps =
  handle (upsertionExceptionHandler (V.toList ps)) $ do
    response <- httpLbs =<< mkRequest
    if statusIsSuccessful (getResponseStatus response)
      then return $ Right (V.length ps,
                           maybe 0 modified_rows $ JSON.decode (getResponseBody response))
      else do print response ; return $ Left (GeneralException (V.toList ps))
  where
    mkRequest :: MonadThrow m => m Request
    mkRequest =
      case murl of
        Nothing ->
          throw $ InvalidEndpointURLException (V.toList ps)

        Just url ->
          (setRequestMethod "POST" .
           setRequestBodyJSON (UpsertRequestBody ps) .
           addRequestHeader "User-Agent" "importer/0.0.1" .
           addRequestHeader "Authorization" ("Bearer " <> configJWT config))
          `fmap` parseRequest (exportURL url)


    murl :: Maybe URL
    murl =
      importURL . unpack $ configDBEndpointURL config <> "/rpc/upsert"


type ExceptionHandler m a =
  (MonadCatch m, MonadIO m) => SomeException -> m a


upsertionExceptionHandler :: a -> ExceptionHandler m (UpsertionResult a)
upsertionExceptionHandler x exception =
  case fromException exception of
    Just e@(HttpExceptionRequest _ _) ->
      do print e ; return $ Left (ConnectionException x)

    e ->
      do print e ; return $ Left (GeneralException x)


type ModificationCount =
  Int


data UpsertionException a
  = GeneralException a
  | ConnectionException a
  | InvalidEndpointURLException a
  deriving (Show, Typeable)


instance (Typeable a, Show a) => Exception (UpsertionException a)


type UpsertionResult a =
  Either (UpsertionException a) (Int, ModificationCount)


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


data UpsertResponseBody =
  UpsertResponseBody { modified_rows :: Int }
  deriving (Generic, Show)


instance JSON.FromJSON UpsertResponseBody where
