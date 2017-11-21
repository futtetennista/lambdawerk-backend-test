{-# LANGUAGE RankNTypes #-}
module Types

where

import Lib.Prelude
import Control.Exception.Safe (MonadCatch)


type ExceptionHandler m a b =
  (MonadCatch m, MonadIO m) => ImporterException a -> m b


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
