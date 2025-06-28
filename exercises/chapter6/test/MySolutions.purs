module Test.MySolutions where

import Prelude

import Data.Array (nub, nubEq, (:))
import Data.Array as Array
import Data.Foldable (class Foldable, fold, foldMap, foldl, foldr, maximum)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromJust)
import Data.Monoid (power)
import Data.Newtype (class Newtype, over, over2, unwrap)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits as String

-- Note to reader: Add your solutions to this file

data Point = Point { x :: Number, y :: Number }

instance Show Point where
  show (Point { x, y }) = "(" <> show x <> ", " <> show y <> ")"

newtype Complex = Complex
  { real :: Number
  , imaginary :: Number
  }

instance Show Complex where
  show (Complex { real, imaginary }) =
    show real
      <>
        ( if 0.0 < imaginary then "+"
          else ""
        )
      <> show imaginary
      <> "i"

-- deligating to the underlying type's instance i.e the Record's Eq
derive newtype instance complexEq :: Eq Complex

-- compiler has builtin in support for deriving Eq 
-- hence we can derive equality for Complex itself
-- derive instance complexEq :: Eq Complex

-- this would be wrong
-- derive newtype instance complexNewtype :: Newtype Complex _
derive instance complexNewtype :: Newtype Complex _

instance Semiring Complex where
  one = Complex { real: 1.0, imaginary: 0.0 }

  zero = Complex { real: 0.0, imaginary: 0.0 }

  -- add (Complex { real: rx, imaginary: ix }) (Complex { real: ry, imaginary: iy }) =
  --   Complex { real: (rx + ry), imaginary: (ix + iy) }

  add = over2 Complex (+)

  mul (Complex { real: rx, imaginary: ix }) (Complex { real: ry, imaginary: iy }) =
    Complex { real, imaginary }
    where
    real = rx * ry - ix * iy
    imaginary = rx * iy + ix * ry

derive newtype instance complexRing :: Ring Complex

data Shape
  = Circle Point Number
  | Line Point Point
  | Rectangle Point Number Number
  | Text Point String

derive instance shapeGeneric :: Generic Shape _

instance Show Shape where
  show = genericShow

data NonEmpty a = NonEmpty a (Array a)

instance (Show a, Show (Array a)) => Show (NonEmpty a) where
  show (NonEmpty x xs) = "NonEmpty " <> show x <> " " <> show xs

-- instance (Eq a, Eq (Array a)) => Eq (NonEmpty a) where
-- note: see below
instance Eq a => Eq (NonEmpty a) where
  eq (NonEmpty a as) (NonEmpty b bs) = eq a b && eq as bs

-- derive instance nonEmptyEq :: (Eq a) => Eq (NonEmpty a)

-- note
-- instance Semigroup (Array a) => Semigroup (NonEmpty a) where
-- Array already has a semigroup instance defined for all `a`
-- this adds a constraint that the sepicify type of a in NonEmpty a
-- have a coresponding Semigroup Array a instance
-- so NonEmpty Int have an Semigroup (Array Int)
-- but Semigroup is already defined for all a so it is redunant
-- still works
instance Semigroup (NonEmpty a) where
  append (NonEmpty x xs) (NonEmpty y ys) = NonEmpty x (xs <> pure y <> ys)

-- note how the type argument for NonEmpty is not provided
instance Functor NonEmpty where
  map mapping (NonEmpty x xs) = NonEmpty (mapping x) (map mapping xs)

data Extended a = Infinite | Finite a

instance Eq a => Eq (Extended a) where
  eq Infinite Infinite = true
  eq _ Infinite = false
  eq Infinite _ = false
  eq (Finite x) (Finite y) = eq x y

instance Ord a => Ord (Extended a) where
  compare Infinite Infinite = EQ
  compare _ Infinite = LT
  compare Infinite _ = GT
  compare (Finite x) (Finite y) = compare x y

instance Foldable NonEmpty where
  foldr f acc (NonEmpty x xs) = f x (foldr f acc xs)

  foldl f acc (NonEmpty x xs) = foldl f (f acc x) xs

  foldMap f (NonEmpty x xs) = f x <> foldMap f xs

data OneMore f a = OneMore a (f a)

instance Foldable f => Foldable (OneMore f) where
  foldr f acc (OneMore x xs) = f x (foldr f acc xs)

  foldl f acc (OneMore x xs) = foldl f (f acc x) xs

  foldMap f (OneMore x xs) = f x <> foldMap f xs

derive instance pointEq :: Eq Point

instance Eq Shape where
  eq (Circle xp xr) (Circle yp yr) = eq xp yp && eq xr yr
  eq (Line xs xe) (Line ys ye) = eq xs ys && eq xe ye
  eq (Rectangle xp xw xh) (Rectangle yp yw yh) = eq xp yp && eq xw yw && eq xh yh
  eq (Text xp xs) (Text yp ys) = eq xp yp && eq xs ys
  eq _ _ = false

dedupShapes :: Array Shape -> Array Shape
dedupShapes = nubEq

derive instance pointOrd :: Ord Point
derive instance shapeOrd :: Ord Shape

dedupShapesFast :: Array Shape -> Array Shape
dedupShapesFast = nub

unsafeMaximum :: Partial => Array Int -> Int
unsafeMaximum = fromJust <<< maximum

newtype Multiply = Multiply Int

derive instance multipyNewtype :: Newtype Multiply _

instance Show Multiply where
  show (Multiply x) = "Multiply " <> show x

instance Semigroup Multiply where
  append (Multiply n) (Multiply m) = Multiply (n * m)

instance Monoid Multiply where
  mempty = Multiply 1

class Monoid m <= Action m a where
  act :: m -> a -> a

instance Action Multiply Int where
  -- implementation 1
  -- act (Multiply x) y = x * y

  -- implementation 2
  -- act mx x = case mx <> Multiply x of
  --   Multiply result -> result

  -- implementation 3
  act mx x =
    (unwrap :: Multiply -> Int)
      $ (over Multiply ((*) x) mx :: Multiply)

instance Action Multiply String where
  act (Multiply times) string = power string times
