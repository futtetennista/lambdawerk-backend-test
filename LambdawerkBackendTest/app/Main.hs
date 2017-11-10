{-# LANGUAGE FlexibleContexts #-}
module Main where

import Protolude
import Person (Person(..), parseFile)
import Conduit
import Data.Vector (Vector)


main :: IO ()
main = do
  batchSize <- calculateBatchSize
  let file = "update-file.xml"
  runConduitRes $ parseFile batchSize file .| sinkDb


calculateBatchSize :: IO Int
calculateBatchSize =
  undefined


sinkDb :: MonadBaseControl IO m => Consumer (Vector Person) (ResourceT m) ()
sinkDb =
  maybe done execDb =<< await
  where
    done =
      return ()

    execDb =
      undefined
