{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Main (main)
where

import Protolude
import Types -- (ImporterResult, ExceptionHandler, exceptionData)
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
                processFile config bs filePath

              _ ->
                printUsage

    _ ->
      printUsage
    where
      processFile :: Database.Config -> Int -> FilePath -> IO ()
      processFile config batchSize fp = do
        putStrLn $ "Processing file: " ++ fp
        -- stream the input file and request UPSERTions asynchronously
        startTime <- getCurrentTime
        asyncUpsertions <- runConduitRes $
          Person.parseXMLInputFile batchSize fp .| manyExecUpsertions config []
        -- wait for all workers to be done and gather their statistics
        results <- waitAll asyncUpsertions
        endTime <- getCurrentTime
        Stats.prettyPrint (Stats.mkStats (startTime, endTime) results)

      printUsage =
        print ("Usage: importer /path/to/xml/file 10000" :: Text)


waitAll :: [IO (ImporterResult (ImporterException (Vector Person)))]
        -> IO [ImporterResult Int]
waitAll as =
  mapConcurrently safeWait as
  where
    safeWait :: IO (ImporterResult (ImporterException (Vector Person)))
             -> IO (ImporterResult Int)
    safeWait a = do
      print ("STARTEd"::Text)
      a' <- a
      either storeEntry strictSuccess a'

    strictSuccess (!x, !y) = do
      print $ ("DONE" :: Text)
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


configFromEnv :: IO (Maybe Database.Config)
configFromEnv =
  liftA2 mkMaybeConfig (lookupEnv "API_ENDPOINT") (lookupEnv "API_TOKEN")
  where
    mkMaybeConfig mendpoint mtoken = do
      endpoint <- mendpoint
      token <- mtoken
      return $ Database.Config (pack endpoint) (pack token)


manyExecUpsertions :: (MonadBaseControl IO m, MonadIO m, MonadCatch m)
               => Database.Config
               -> [m (ImporterResult (ImporterException (Vector Person)))]
               -> Consumer (Vector Person) (ResourceT m) [m (ImporterResult (ImporterException (Vector Person)))]
manyExecUpsertions config asyncUpsertions = do
  mpersons <- await
  case mpersons of
    Nothing ->
      done

    Just persons ->
      -- process this batch and loop
      manyExecUpsertions config =<< execUpsertion persons
  where
    done =
      return asyncUpsertions

    execUpsertion persons =
      return $ (Database.merge config persons) : asyncUpsertions
