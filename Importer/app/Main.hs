{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Main (main)
where

import Protolude
import Types (BatchSize, ImporterResult, exceptionData)
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
                runReaderT mergeProcess (config, bs, filePath)

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


type Env =
  (Database.Config, BatchSize, FilePath)


type Importer =
  ReaderT Env


mergeProcess :: (MonadIO m) => Importer m ()
mergeProcess = do
  startTime <- liftIO getCurrentTime
  results <- runMerge
  endTime <- liftIO getCurrentTime
  Stats.prettyPrint (Stats.mkStats (startTime, endTime) results)


-- The merge process is asynchronous and runs in constant memory:
-- each time `batchSize` entries are read from the XML input file
-- a new merge action is executed
runMerge :: (MonadIO m) => Importer m [ImporterResult Int]
runMerge = do
  (config, batchSize, fp) <- ask
  liftIO . runConduitRes $
    Person.parseXMLInputFile batchSize fp
      .| manyExecMerge config
      .| sinkList


manyExecMerge :: (MonadIO m)
              => Database.Config
              -> Conduit (Vector Person) (ResourceT m) (ImporterResult Int)
manyExecMerge config =
  fix $ \loop -> maybe done (\xs -> yieldMergeResult xs >> loop) =<< await
  where
    done :: (MonadIO m) => Conduit (Vector Person) (ResourceT m) (ImporterResult Int)
    done =
      return ()

    yieldMergeResult :: (MonadIO m)
                    => Vector Person -> Conduit (Vector Person) (ResourceT m) (ImporterResult Int)
    yieldMergeResult persons =
      yield =<< liftIO (execMerge persons)

    execMerge persons =
      waitResult =<< async (Database.merge config persons)
      where
        waitResult a =
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
