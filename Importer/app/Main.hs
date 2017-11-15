{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Main (main)
where

import Protolude hiding (first, second)
import Person (Person(..), parseInputFile)
import Database
import Conduit
import Data.Vector (Vector)
import System.Environment (lookupEnv)
import Data.ByteString.Char8 (pack)
import Control.Arrow (first, second)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)


main :: IO ()
main = do
  batchSize <- calculateBatchSize
  filePaths <- getArgs
  if null filePaths
    then print ("No input file provided" :: Text)
    else do
      mconfig <- configFromEnv
      case mconfig of
          Nothing ->
            print ("'API_ENDPOINT' or/and 'API_TOKEN' env variables not set. Cannot connect to the db." :: Text)

          Just config ->
            forM_ filePaths (processFile config batchSize)
    where
      processFile :: Config -> Int -> FilePath -> IO ()
      processFile config batchSize fp = do
        putStrLn $ "Processing file: " ++ fp
        -- stream the input file and request UPSERTions asynchronously
        startTime <- getCurrentTime
        asyncUpsertions <- runConduitRes $
          parseInputFile batchSize fp .| execUpsertions config []
        -- wait for all workers to be done and gather their statistics
        stats <- waitAll upsertionExceptionHandler asyncUpsertions
        endTime <- getCurrentTime
        printStats (startTime, endTime) stats


waitAll :: ExceptionHandler IO a -> [Async a] -> IO [a]
waitAll handler =
  foldr (liftA2 (:) . tryWait) (return [])
  where
    tryWait =
      handle handler . wait


printStats :: (UTCTime, UTCTime) -> [UpsertionResult] -> IO ()
printStats (startTime, endTime) stats = do
  let
    (failureCount, successCount) =
      foldr accumulateResults (0, 0) stats
  print ("Failures: " <> show failureCount <> "\tSuccesses:" <> show successCount :: Text)
  print ("importer took " <> show (diffUTCTime endTime startTime)
         <> " seconds to import ??? members" :: Text)
  where
    accumulateResults :: UpsertionResult -> (Int, Int) -> (Int, Int)
    accumulateResults eres acc =
      either (const $ first (+1) acc) (\upsertionCount -> second (+upsertionCount) acc) eres


configFromEnv :: IO (Maybe Config)
configFromEnv =
  liftA2 mkMaybeConfig (lookupEnv "API_ENDPOINT") (lookupEnv "API_TOKEN")
  where
    mkMaybeConfig mendpoint mtoken = do
      endpoint <- mendpoint
      token <- mtoken
      return $ Config (pack endpoint) (pack token)


-- https://stackoverflow.com/questions/3254758/memory-footprint-of-haskell-data-types
calculateBatchSize :: IO Int
calculateBatchSize =
  return 1000


execUpsertions :: (MonadBaseControl IO m, MonadIO m)
               => Config
               -> [Async UpsertionResult]
               -> Consumer (Vector Person) (ResourceT m) [Async UpsertionResult]
execUpsertions config asyncUpsertions = do
  mpersons <- await
  case mpersons of
    Nothing ->
      done

    Just persons ->
      -- process this batch and loop
      execUpsertions config =<< execUpsertion persons
  where
    done =
      return asyncUpsertions

    execUpsertion persons = do
      asyncUpsertion <- liftIO $ async (upsert config persons)
      return $ asyncUpsertion : asyncUpsertions
      -- liftA2 (:) (liftIO $ async (upsert personBatch)) (pure asyncUpsertions)
