{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Main (main)
where

import Protolude
import Types (ImporterResult, exceptionData)
import Person (Person)
import qualified Person
import qualified Database
import qualified Stats
import Conduit
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.Environment (lookupEnv)
import Data.ByteString.Char8 (pack)
import Data.Time.Clock (getCurrentTime)
import Control.Exception.Safe
import Control.Monad.Fix (fix)
import GHC.Base (String)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath, batchSize] ->
      if null filePath
      then print ("No input file provided" :: Text)
      else do
        mconfig <- configFromEnv
        case mconfig of
          Nothing ->
            print ("'API_ENDPOINT' or/and 'API_TOKEN' env variables not set. Cannot connect to the db." :: Text)

          Just config ->
            case reads batchSize of
              [(bs, "")] -> do
                print ("Config: " <> show config :: Text)
                print ("Batch size: " <> show bs :: Text)
                print ("Processing file: " ++ filePath :: String)
                processFile config bs filePath

              _ ->
                printUsage

    _ ->
      printUsage
    where
      printUsage =
        print ("Usage: importer /path/to/xml/file 10000" :: Text)


configFromEnv :: IO (Maybe Database.Config)
configFromEnv =
  liftA2 mkMaybeConfig (lookupEnv "API_ENDPOINT") (lookupEnv "API_TOKEN")
  where
    mkMaybeConfig mendpoint mtoken = do
      endpoint <- mendpoint
      token <- mtoken
      return $ Database.Config (pack endpoint) (pack token)


processFile :: (MonadBaseControl IO m, MonadIO m, MonadCatch m)
            => Database.Config -> Int -> FilePath -> m ()
processFile config batchSize fp = do
  startTime <- liftIO getCurrentTime
  results <- runMerge batchSize config fp
  endTime <- liftIO getCurrentTime
  Stats.prettyPrint (Stats.mkStats (startTime, endTime) results)


-- The merge process is asynchronous and runs in constant memory:
-- each time `batchSize` entries are read from the XML input file
-- a new merge action is executed
runMerge :: (MonadBaseControl IO m, MonadIO m, MonadCatch m)
         => Int -> Database.Config -> FilePath -> m [ImporterResult Int]
runMerge batchSize config fp =
  liftIO . runConduitRes $
    Person.parseXMLInputFile batchSize fp
      .| manyExecUpsertions config
      .| sinkList


manyExecUpsertions :: (MonadBaseControl IO m, MonadIO m, MonadCatch m)
                   => Database.Config
                   -> Conduit (Vector Person) (ResourceT m) (ImporterResult Int)
manyExecUpsertions config =
  fix $ \loop -> maybe done (\xs -> yieldExecResult xs >> loop) =<< await
  where
    done :: (MonadBaseControl IO m, MonadIO m, MonadCatch m)
         => Conduit (Vector Person) (ResourceT m) (ImporterResult Int)
    done =
      return ()

    yieldExecResult :: (MonadBaseControl IO m, MonadIO m, MonadCatch m)
                    => Vector Person
                    -> Conduit (Vector Person) (ResourceT m) (ImporterResult Int)
    yieldExecResult persons =
      yield =<< liftIO (execMerge persons)

    -- execMerge :: (MonadBaseControl IO m, MonadIO m, MonadCatch m)
    --           => Vector Person
    --           -> m (ImporterResult Int)
    execMerge persons =
      safeWait =<< async (Database.merge config persons)
      where
        -- safeWait :: (MonadBaseControl IO m, MonadIO m, MonadCatch m)
        --          => Async (ImporterResult (ImporterException (Vector Person)))
        --          -> m (ImporterResult Int)
        safeWait a =
          either storeEntry strictSuccess =<< wait a

        strictSuccess (!x, !y) =
          return $ Right (x, y)

        storeEntry ex =
          let
            ps =
              Types.exceptionData ex

            !l =
              V.length ps
          -- TODO: write entries to "update-failed.xml" file
          in
            return $ Left l
