module Test.MySolutions where

import Prelude

import Data.Person (Person)

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

pascal :: Int -> Int -> Int
pascal _ 0 = 1
pascal n k
  | n == k = 1
  | otherwise = bA + bB
      where
      bA = binomial (n - 1) k
      bB = binomial (n - 1) (k - 1)

sameCity :: Person -> Person -> Boolean
sameCity { address: { city: cityA } } { address: { city: cityB } } = cityA == cityB

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [ only ] = only
fromSingleton default _ = default
