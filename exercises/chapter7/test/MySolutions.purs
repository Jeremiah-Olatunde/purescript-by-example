module Test.MySolutions where

import Prelude

import Control.Apply (lift2)
import Data.AddressBook (Address, address)
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex, test)
import Data.String.Regex.Flags (global, ignoreCase)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Validation.Semigroup (V, invalid)

-- Note to reader: Add your solutions to this file

addMaybe :: forall a. Semiring a => Maybe a -> Maybe a -> Maybe a
addMaybe = lift2 (+)

mulMaybe :: forall a. Semiring a => Maybe a -> Maybe a -> Maybe a
mulMaybe = lift2 (*)

subMaybe :: forall a. Ring a => Maybe a -> Maybe a -> Maybe a
subMaybe = lift2 (-)

divMaybe :: forall a. EuclideanRing a => Maybe a -> Maybe a -> Maybe a
divMaybe = lift2 (/)

addApply :: forall f a. Apply f => Semiring a => f a -> f a -> f a
addApply = lift2 (+)

mulApply :: forall f a. Apply f => Semiring a => f a -> f a -> f a
mulApply = lift2 (*)

subApply :: forall f a. Apply f => Ring a => f a -> f a -> f a
subApply = lift2 (-)

divApply :: forall f a. Apply f => EuclideanRing a => f a -> f a -> f a
divApply = lift2 (/)

combineMaybe :: forall f a. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing
combineMaybe (Just fa) = map Just fa

stateRegex :: Regex
stateRegex = unsafeRegex "^[a-z][a-z]$" (global <> ignoreCase)

nonEmptyRegex :: Regex
nonEmptyRegex = unsafeRegex ".*\\S.*" (global)

validateWithRegex :: Regex -> String -> String -> V (Array String) String
validateWithRegex regex fieldName unvalidated =
  if test regex unvalidated then pure unvalidated
  else invalid [ "Field '" <> fieldName <> "' did not match the required format" ]

validateAddressImproved :: Address -> V (Array String) Address
validateAddressImproved a = ado
  street <- validateWithRegex nonEmptyRegex "Street" a.street
  city <- validateWithRegex nonEmptyRegex "City" a.city
  state <- validateWithRegex stateRegex "State" a.state
  in address street city state

data Tree a = Leaf | Branch (Tree a) a (Tree a)

derive instance treeEq :: (Eq a) => Eq (Tree a)

instance (Show a) => Show (Tree a) where
  show Leaf = "Leaf"
  show (Branch left value right) =
    "(Branch"
      <> " "
      <> show left
      <> " "
      <> show value
      <> " "
      <> show right
      <> ")"
