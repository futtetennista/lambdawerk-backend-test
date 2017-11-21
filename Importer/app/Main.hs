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
          Person.parseXMLInputFile batchSize fp .| execUpsertions config []
        -- wait for all workers to be done and gather their statistics
        results <- waitAll asyncUpsertions
        endTime <- getCurrentTime
        Stats.prettyPrint (Stats.mkStats (startTime, endTime) results)

      printUsage =
        print ("Usage: importer /path/to/xml/file 10000" :: Text)


waitAll :: [Async (ImporterResult (ImporterException (Vector Person)))]
        -> IO [ImporterResult Int]
waitAll =
  foldr (liftA2 (:) . safeWait) (return [])
  where
    safeWait :: Async (ImporterResult (ImporterException (Vector Person)))
            -> IO (ImporterResult Int)
    safeWait a =
      either handler (return . Right) =<< wait a

    handler :: MonadIO m => ExceptionHandler m (Vector Person) (ImporterResult Int)
    handler ex =
      -- TODO: write entries to "update-failed.xml" file
      return $ Left (V.length $ Types.exceptionData ex)


configFromEnv :: IO (Maybe Database.Config)
configFromEnv =
  liftA2 mkMaybeConfig (lookupEnv "API_ENDPOINT") (lookupEnv "API_TOKEN")
  where
    mkMaybeConfig mendpoint mtoken = do
      endpoint <- mendpoint
      token <- mtoken
      return $ Database.Config (pack endpoint) (pack token)


execUpsertions :: (MonadBaseControl IO m, MonadIO m)
               => Database.Config
               -> [Async (ImporterResult (ImporterException (Vector Person)))]
               -> Consumer (Vector Person) (ResourceT m) [Async (ImporterResult (ImporterException (Vector Person)))]
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
      asyncUpsertion <- liftIO $ async (Database.merge config persons)
      return $ asyncUpsertion : asyncUpsertions
      -- liftA2 (:) (liftIO $ async (upsert personBatch)) (pure asyncUpsertions)
