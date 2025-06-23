module Test.MySolutions where

import Prelude

import Data.Ord (abs)

-- Note to reader: Add your solutions to this file

isEven :: Int -> Boolean
isEven = isEven' <<< abs
  where
  isEven' :: Int -> Boolean
  isEven' 0 = true
  isEven' 1 = false
  isEven' n = isEven' (n - 2)
