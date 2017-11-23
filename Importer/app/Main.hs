{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Main (main)
where

import Protolude
import Types (BatchSize, exceptionData)
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
import GHC.Conc (numCapabilities)
import Control.Concurrent.STM.TChan (TChan, newTChan, writeTChan, readTChan)
import Control.Concurrent.STM.TVar (TVar, newTVar, modifyTVar', readTVarIO)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath, batchSize] ->
      if null filePath
      then print ("No input file provided" :: Text)
      else do
        print ("Merge process started. Processing entries in XML input file: '" ++ filePath ++ "'")
        maybe (return ()) (runReaderT mergeProcess) =<< getMergeProcessEnv filePath batchSize

    _ ->
      printUsage
    where
      printUsage =
        print ("Usage: importer /path/to/xml/file 10000" :: Text)

      getMergeProcessEnv fp bs = do
        mdbConfig <- getDBConfig
        case mdbConfig of
          Nothing -> do
            print ("'API_ENDPOINT' or/and 'API_TOKEN' env variables not set. Cannot connect to the db." :: Text)
            return Nothing

          Just dbConfig ->
            case reads bs of
              [(n, "")] -> do
                (q, s) <- atomically $ do q <- newTChan
                                          s <- newTVar Stats.emptyStats
                                          return (q, s)
                let
                  env =
                    Env (dbConfig, n, fp) q s
                return $ Just env

              _ ->
                printUsage >> return Nothing


getDBConfig :: IO (Maybe Database.Config)
getDBConfig =
  liftA2 mkMaybeConfig (lookupEnv "API_ENDPOINT") (lookupEnv "API_TOKEN")
  where
    mkMaybeConfig mendpoint mtoken = do
      endpoint <- mendpoint
      token <- mtoken
      return $ Database.Config (pack endpoint) (pack token)


data Env =
  Env { config :: Config
      , queue :: Queue
      , stats :: TVar Stats.Stats
      }


type Config =
  (Database.Config, BatchSize, FilePath)


type Queue =
  TChan (Job (Vector Person))


data Job a
  = Done
  | Batch a


type Importer =
  ReaderT Env


mergeProcess :: (MonadIO m) => Importer m ()
mergeProcess = do
  start <- liftIO getCurrentTime
  runMergeProcess
  end <- liftIO getCurrentTime
  updateDuration start end
  Stats.prettyPrint =<< readStats
  where
    readStats =
      liftIO . readTVarIO =<< asks stats

    updateDuration start end = do
      mergeProcessStats <- asks stats
      liftIO $ atomically (modifyTVar' mergeProcessStats (Stats.updateDuration start end))


-- The merge process is asynchronous and runs in constant memory:
-- each time `batchSize` entries are read from the XML input file
-- a new merge process job is put in the queue and eventually processed
-- by a worker.
runMergeProcess :: (MonadIO m) => Importer m ()
runMergeProcess = do
  (c, n, fp) <- asks config
  q <- asks queue
  s <- asks stats
  liftIO $ do workers <- mkWorkers c q s
              streamXMLInputFile n fp q
              mapM_ wait workers
  where
    streamXMLInputFile :: BatchSize -> FilePath -> Queue -> IO ()
    streamXMLInputFile n fp q =
      runConduitRes $
        Person.parseXMLInputFile n fp
          .| manyExecMerge q
          .| sinkNull

    -- the concurrency degree is by looking at the number of Haskell threads
    -- that can run truly simultaneously
    mkWorkers :: Database.Config -> Queue -> TVar Stats.Stats -> IO [Async ()]
    mkWorkers c q s =
      replicateM numCapabilities (async (worker c q s))


-- Waits for a new merge process job, runs it and updates the stats. It stops when no more
-- merge process jobs are present in the queue
worker :: (MonadIO m) => Database.Config -> Queue -> TVar Stats.Stats -> m ()
worker c q s = do
  work <- liftIO $ atomically (readTChan q)
  case work of
    Done ->
      return ()

    Batch ps -> do
      liftIO (updateResults =<< execMerge ps)
      worker c q s
  where
    updateResults =
      atomically . modifyTVar' s . Stats.updateResults

    execMerge persons =
      waitResult =<< async (Database.merge c persons)

    waitResult asyncAction =
      either storeEntry strictlySuccess =<< wait asyncAction

    strictlySuccess (!x, !y) =
      return $ Right (x, y)

    -- TODO: write entries to "update-failed.xml" file
    storeEntry ex =
      let
        !l =
          V.length $ Types.exceptionData ex
      in
        return $ Left l


-- Each time `batchSize` entries are read and parsed from the XML input file create a
-- new merge process job and enqueue it to be run by the next available worker
manyExecMerge :: (MonadIO m) => Queue -> Conduit (Vector Person) (ResourceT m) ()
manyExecMerge q =
  fix $ \loop -> maybe done (\xs -> yieldMergeResult xs >> loop) =<< await
  where
    done =
      void stopAllWorkers

    stopAllWorkers =
      replicateM numCapabilities $ enqueueMergeJob Done

    yieldMergeResult persons =
      enqueueMergeJob (Batch persons) >> yield ()

    enqueueMergeJob =
      liftIO . atomically . writeTChan q
