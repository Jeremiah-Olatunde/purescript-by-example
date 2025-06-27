module Test.MySolutions where

import Prelude

import Data.Newtype (class Newtype, over2)

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

