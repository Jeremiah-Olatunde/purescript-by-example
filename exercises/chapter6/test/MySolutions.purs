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

