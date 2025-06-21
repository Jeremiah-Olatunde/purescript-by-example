module Test.MySolutions where

import Prelude

import Data.Int (rem)
import Data.Number (sqrt, pi)

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)

circleArea :: Number -> Number
circleArea radius = pi * radius * radius

leftoverCents :: Int -> Int
leftoverCents n = rem n 100
