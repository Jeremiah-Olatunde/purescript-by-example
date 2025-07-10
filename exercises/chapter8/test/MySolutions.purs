module Test.MySolutions where

import Prelude

import Control.MonadPlus (guard)
import Data.Array (nub, sort, tail, head, foldM)
import Data.List (List(Nil), (:))
import Data.Maybe (Maybe)

-- Note to reader: Add your solutions to this file

third :: forall a. Array a -> Maybe a
third xs = do
  from1 <- tail xs
  from2 <- tail from1
  head from2

possibleSums :: Array Int -> Array Int
possibleSums = sort <<< nub <<< foldM (\b a -> [ a, b, a + b ]) 0

filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM _ Nil = pure Nil
filterM f (x : xs) = do
  bool <- f x
  ys <- filterM f xs
  if bool then pure $ x : ys
  else pure ys
