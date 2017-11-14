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
        asyncUpsertions <- runConduitRes $
          parseInputFile batchSize fp .| execUpsertions config []
        results <- waitAll asyncUpsertions
        printResults results


-- Wait for all workers to be done, ignoring failures
waitAll :: [Async a] -> IO [Either SomeException a]
waitAll [] =
  return []
waitAll (a:as) =
  liftA2 (:) (waitCatch a) (waitAll as)


printResults :: [Either SomeException UpsertionResult] -> IO ()
printResults =
  print . foldr (\es acc -> either (const $ first (+1) acc)
                                   (either (const $ first (+1) acc) (\x -> second (+x) acc))
                                   es)
                ((0, 0) :: (Int, Int))



configFromEnv :: IO (Maybe Config)
configFromEnv =
  liftA2 mkMaybeConfig (lookupEnv "API_ENDPOINT") (lookupEnv "API_TOKEN")
  where
    mkMaybeConfig :: Maybe [Char] -> Maybe [Char] -> Maybe Config
    mkMaybeConfig mendpoint mjwtToken = do
      endpoint <- mendpoint
      jwtToken <- mjwtToken
      return $ Config (pack endpoint) (pack jwtToken)


-- https://stackoverflow.com/questions/3254758/memory-footprint-of-haskell-data-types
calculateBatchSize :: IO Int
calculateBatchSize =
  return 5000


execUpsertions :: (MonadBaseControl IO m, MonadIO m)
               => Config
               -> [Async UpsertionResult]
               -> Consumer (Vector Person) (ResourceT m) [Async UpsertionResult]
execUpsertions config asyncUpsertions =
  maybe done execUpsertion =<< await
  where
    done =
      return asyncUpsertions

    execUpsertion personBatch = do
      asyncUpsertion <- liftIO $ async (upsert config personBatch)
      return $ asyncUpsertion : asyncUpsertions
      -- liftA2 (:) (liftIO $ async (upsert personBatch)) (pure asyncUpsertions)
