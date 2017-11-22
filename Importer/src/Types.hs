{-# LANGUAGE RankNTypes #-}
module Types

where

import Lib.Prelude


type BatchSize =
  Int


type RowStats =
  Int


type ImporterResult a =
  Either a (Int, RowStats)


data ImporterException a
  = GeneralException { exceptionData :: a }
  | ConnectionException { exceptionData :: a }
  | InvalidEndpointURLException { exceptionData :: a }
  deriving (Show, Typeable)


instance (Typeable a, Show a) => Exception (ImporterException a)
