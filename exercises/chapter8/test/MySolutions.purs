module Test.MySolutions where

import Prelude

import Data.Maybe (Maybe)
import Data.Array (nub, sort, tail, head, foldM)

-- Note to reader: Add your solutions to this file

third :: forall a. Array a -> Maybe a
third xs = do
  from1 <- tail xs
  from2 <- tail from1
  head from2

possibleSums :: Array Int -> Array Int
possibleSums = sort <<< nub <<< foldM (\b a -> [ a, b, a + b ]) 0
