{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
module Database ( MergeResult
                , MergeException
                , RowStats
                , Config(..)
                , ExceptionHandler
                , merge
                , mergeExceptionHandler
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


merge :: (MonadCatch m, MonadIO m) => Config -> Vector Person -> m (MergeResult [Person])
merge config ps =
  handle (mergeExceptionHandler (V.toList ps)) $ do
    response <- httpLbs =<< mkRequest
    if statusIsSuccessful (getResponseStatus response)
      then return $ Right (V.length ps,
                           maybe 0 row_stats $ JSON.decode (getResponseBody response))
      else do print response ; return $ Left (GeneralException (V.toList ps))
  where
    mkRequest :: MonadThrow m => m Request
    mkRequest =
      case murl of
        Nothing ->
          throw $ InvalidEndpointURLException (V.toList ps)

        Just url ->
          (setRequestMethod "POST" .
           setRequestBodyJSON (MergeRequestBody ps) .
           addRequestHeader "User-Agent" "importer/0.0.1" .
           addRequestHeader "Authorization" ("Bearer " <> configJWT config))
          `fmap` parseRequest (exportURL url)


    murl :: Maybe URL
    murl =
      importURL . unpack $ configDBEndpointURL config <> "/rpc/upsert"


type ExceptionHandler m a =
  (MonadCatch m, MonadIO m) => SomeException -> m a


mergeExceptionHandler :: a -> ExceptionHandler m (MergeResult a)
mergeExceptionHandler x exception =
  case fromException exception of
    Just e@(HttpExceptionRequest _ _) ->
      do print e ; return $ Left (ConnectionException x)

    e ->
      do print e ; return $ Left (GeneralException x)


type RowStats =
  Int


data MergeException a
  = GeneralException a
  | ConnectionException a
  | InvalidEndpointURLException a
  deriving (Show, Typeable)


instance (Typeable a, Show a) => Exception (MergeException a)


type MergeResult a =
  Either (MergeException a) (Int, RowStats)


data Config =
  Config { configDBEndpointURL :: ByteString
         , configJWT :: ByteString
         }
  deriving Show


newtype MergeRequestBody =
  MergeRequestBody { entries :: Vector Person }
  deriving (Generic, Show)


-- Automatic (and fast!) decoding to corresponding JSON object
instance JSON.ToJSON MergeRequestBody where
  toEncoding =
    JSON.genericToEncoding JSON.defaultOptions


{-# ANN MergeResponseBody ("HLint: ignore Use camelCase" :: Text) #-}
newtype MergeResponseBody =
  MergeResponseBody { row_stats :: Int }
  deriving (Generic, Show)


instance JSON.FromJSON MergeResponseBody where
