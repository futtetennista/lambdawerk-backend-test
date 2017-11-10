module Main where

import Protolude
import Person
import Conduit
import qualified Data.Vector as V

main :: IO ()
main =
  undefined


httpSink :: Sink (V.Vector Person) (ResourceT IO) ()
httpSink = do
  xs <- await
  maybe (return ()) print xs
