{-# LANGUAGE RankNTypes #-}
module Types

where

import Lib.Prelude


type BatchSize =
  Int


type RowStats =
  Int


type MergeJobResult a =
  Either a (Int, RowStats)


data MergeJobException a
  = GeneralException { exceptionData :: a }
  | ConnectionException { exceptionData :: a }
  | InvalidEndpointURLException { exceptionData :: a }
  deriving (Show, Typeable)


instance (Typeable a, Show a) => Exception (MergeJobException a)
