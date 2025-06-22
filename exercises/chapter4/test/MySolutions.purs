module Test.MySolutions where

import Prelude

factorial :: Int -> Int
factorial n
  | n < 1 = 1
  | otherwise = n * factorial (n - 1)

binomial :: Int -> Int -> Int
binomial n k = fN / (fK * fNK)
  where
  fN = factorial n
  fK = factorial k
  fNK = factorial (n - k)
