module Test.MySolutions where

import Prelude

import Data.Ord (abs)
import Data.Array (head, tail)
import Data.Maybe (fromMaybe)

-- Note to reader: Add your solutions to this file

isEven :: Int -> Boolean
isEven = isEven' <<< abs
  where
  isEven' :: Int -> Boolean
  isEven' 0 = true
  isEven' 1 = false
  isEven' n = isEven' (n - 2)

countEven :: Array Int -> Int
countEven [] = 0
countEven ns = add increment $ countEven $ fromMaybe [] $ tail ns
  where
  oneIfEven n = if mod n 2 == 0 then 1 else 0
  increment = fromMaybe 0 $ map oneIfEven $ (head ns)

