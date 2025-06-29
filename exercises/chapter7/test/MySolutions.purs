module Test.MySolutions where

import Prelude
import Data.Maybe (Maybe)
import Control.Apply (lift2)

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

