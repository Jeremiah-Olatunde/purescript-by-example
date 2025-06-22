module Test.MySolutions where

import Prelude

import Data.Person (Person)
import Data.Picture (Shape(..), Point)
import Data.Maybe (Maybe(Just, Nothing))

import ChapterExamples (Amp(..), Volt(..))

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

origin :: Point
origin = { x: 0.0, y: 0.0 }

circleAtOrigin :: Shape
circleAtOrigin = Circle origin 10.0

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter (Text _ text) = Text origin text
doubleScaleAndCenter (Circle _ radius) = Circle origin (radius * 2.0)
doubleScaleAndCenter (Rectangle _ w h) = Rectangle origin (w * 2.0) (h * 2.0)
doubleScaleAndCenter (Line start end) = Line startNext endNext
  where
  dx :: Number
  dx = end.x - start.x

  dy :: Number
  dy = end.y - start.y

  startNext :: Point
  startNext = { x: (-dx), y: (-dy) }

  endNext :: Point
  endNext = { x: dx, y: dy }

shapeText :: Shape -> Maybe String
shapeText (Text _ text) = Just text
shapeText _ = Nothing

newtype Watt = Watt Number

calculateWattage :: Amp -> Volt -> Watt
calculateWattage (Amp current) (Volt voltage) = Watt (current * voltage)
