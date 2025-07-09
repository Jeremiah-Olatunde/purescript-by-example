module Test.MySolutions where

import Prelude

import Data.Maybe (Maybe)
import Data.Array as Array

-- Note to reader: Add your solutions to this file

third :: forall a. Array a -> Maybe a
third xs = do
  from1 <- Array.tail xs
  from2 <- Array.tail from1
  Array.head from2
