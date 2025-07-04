module Test.MySolutions where

import Prelude

import Control.Alternative (guard)
import Data.Array (all, any, concatMap, cons, filter, fold, foldl, head, last, length, sortBy, tail, (..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (abs)
import Data.Path (Path(..), filename, ls, size)
import Data.String (Pattern(..), contains)
import Test.Examples (factors)

-- Note to reader: Add your solutions to this file

isEven :: Int -> Boolean
isEven = isEven' <<< abs
  where
  isEven' :: Int -> Boolean
  isEven' 0 = true
  isEven' 1 = false
  isEven' n = isEven' (n - 2)

countEven :: Array Int -> Int
countEven [] = 0
countEven ns = add increment $ countEven $ fromMaybe [] $ tail ns
  where
  oneIfEven n = if mod n 2 == 0 then 1 else 0
  increment = fromMaybe 0 $ map oneIfEven $ (head ns)

squared :: Array Number -> Array Number
squared = map square
  where
  square n = n * n

keepNonNegative :: Array Number -> Array Number
keepNonNegative = filter $ (<=) 0.0

infix 8 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite = (<$?>) $ (<=) 0.0

isPrime :: Int -> Boolean
isPrime 0 = false
isPrime 1 = false
isPrime n = eq 1 $ length $ factors n

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct xs ys = do
  x <- xs
  y <- ys
  pure [ x, y ]

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- a .. n
  c <- b .. n
  guard $ c * c == a * a + b * b
  pure [ a, b, c ]

primeFactors :: Int -> Array Int
primeFactors n = primeFactors' 2 n
  where
  primeFactors' :: Int -> Int -> Array Int
  primeFactors' _ 1 = []
  primeFactors' p m
    | isPrime p && mod m p == 0 = cons p $ primeFactors' p (m / p)
    | otherwise = primeFactors' (p + 1) m

allTrue :: Array Boolean -> Boolean
allTrue = foldl (&&) true

fibTailRec :: Int -> Int
fibTailRec = fib' 0 1
  where
  fib' :: Int -> Int -> Int -> Int
  fib' x _ 0 = x
  fib' x y m = fib' y (x + y) (m - 1)

reverse :: forall a. Array a -> Array a
reverse = foldl (flip cons) []

allFiles :: Path -> Array Path
allFiles path = path : do
  files <- ls path
  allFiles files

onlyFiles :: Path -> Array Path
onlyFiles file@(File _ _) = pure file
onlyFiles directory = concatMap onlyFiles (ls directory)

whereIs :: Path -> String -> Maybe Path
whereIs path target = head $ do
  item <- ls path
  if filename path <> target == filename item then pure path
  else fromMaybe [] $ map pure $ whereIs item target

fileInDir :: Array Path -> String -> Boolean
fileInDir items target = any (contains (Pattern target) <<< filename) items

largestSmallest :: Path -> Array Path
largestSmallest = select <<< sortedFiles

select :: forall a. Array a -> Array a
select [] = []
select [ first ] = [ first ]
select xs = fromMaybe [] $ (h <> l)
  where
  h = map (pure :: a -> Array a) $ head xs
  l = map (pure :: a -> Array a) $ last xs

sortedFiles :: Path -> Array Path
sortedFiles path = sortBy cmpPath $ onlyFiles path

cmpPath :: Path -> Path -> Ordering
cmpPath px py = compare (size px) (size py)

