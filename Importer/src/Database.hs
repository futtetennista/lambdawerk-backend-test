{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
module Database ( Config(..)
                , merge
                )
where

import Lib.Prelude hiding (handle)
import Types (ImporterResult, ImporterException(..))
import Person (Person(..))
import Data.Vector (Vector)
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import Control.Exception.Safe
import Network.URL
import Data.ByteString.Char8 (unpack)
import qualified Data.Aeson as JSON
import qualified Data.Vector as V


merge :: (MonadCatch m, MonadIO m)
      => Config
      -> Vector Person
      -> m (ImporterResult (ImporterException (Vector Person)))
merge config ps =
  handle exceptionHandler $ do
    response <- httpLbs =<< mkRequest
    if statusIsSuccessful (getResponseStatus response)
      then
        let
          mdecodedResponseBody =
            JSON.decode (getResponseBody response)
        in
          return $ Right (V.length ps, rowStatsOrZero mdecodedResponseBody)
      else throw $ GeneralException ps
  where
    rowStatsOrZero =
      maybe 0 row_stats

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

    -- take into account the possibility that the supplied URL is invalid
    murl :: Maybe URL
    murl =
      importURL . unpack $ configDBEndpointURL config <> "/rpc/merge"

    exceptionHandler exception =
      case fromException exception of
        Just e@(HttpExceptionRequest _ _) ->
          do print e ; throw (Types.ConnectionException ps)

        e ->
          do print e ; throw (Types.GeneralException ps)


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
