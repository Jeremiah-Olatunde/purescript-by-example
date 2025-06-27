module Test.MySolutions where

import Prelude

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

derive instance complex :: Eq Complex

instance Semiring Complex where
  one = Complex { real: 1.0, imaginary: 0.0 }

  zero = Complex { real: 0.0, imaginary: 0.0 }

  add (Complex { real: rx, imaginary: ix }) (Complex { real: ry, imaginary: iy }) =
    Complex { real: (rx + ry), imaginary: (ix + iy) }

  mul (Complex { real: rx, imaginary: ix }) (Complex { real: ry, imaginary: iy }) =
    Complex { real, imaginary }
    where
    real = rx * ry - ix * iy
    imaginary = rx * iy + ix * ry

