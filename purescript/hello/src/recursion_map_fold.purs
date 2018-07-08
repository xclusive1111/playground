module Data.Op where

import Prelude
import Data.Array
import Data.Array.Partial (tail, head)
import Partial.Unsafe (unsafePartial)
import Control.MonadZero (guard)
import Data.Foldable (product)
import Data.Tuple

length :: forall a. Array a -> Int
length arr =
  if null arr
    then 0
    else 1 + length (unsafePartial tail arr)


isEven :: Int -> Boolean
isEven n = mod n 2 == 0

countEven :: Array Int -> Int
countEven arr = length $ filter isEven arr

square :: Int -> Int
square n = n * n

squareArr :: Array Int -> Array Int
squareArr = map square

pairs :: Int -> Array (Array Int)
pairs n = concatMap (\i -> map (\j -> [i, j]) (i .. n)) (1 .. n)

factors :: Int -> Array (Array Int)
factors n = filter (\xs -> product xs == n) $ do
  i <- 1 .. n
  j <- 1 .. n
  pure [i, j]

-- Using guard instead of filter
factors' :: Int -> Array (Array Int)
factors' n = do
  i <- 1 .. n
  j <- 1 .. n
  guard $ i * j == n
  pure [i, j]

isPrime :: Int -> Boolean
isPrime n = length (factors n) == 2

cartesianProduct :: forall a b. Array a -> Array b -> Array (Tuple a b)
cartesianProduct setA setB = do
  a <- setA
  b <- setB
  pure (Tuple a b)

-- Pythagorean triple, such that [a, b, c] where a2 + b2 = c2
-- Takes a number n and calculates all Pythagorean triples
-- whose components are less than n
triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- 1 .. n
  c <- 1 .. n
  guard $ square a + square b == square c
  pure [a, b, c]

-- Produces all factorizations of a given integer
factorizations :: Int -> Array (Array Int)
factorizations n = [n] : do
  x  <- filter (\x -> x > 1 && x < n) $ actualFactors n
  ys <- factorizations (n / x)
  pure (x : ys)

actualFactors :: Int -> Array Int
actualFactors n = do
  i <- 1 .. n
  guard $ n `mod` i == 0
  pure i

-- Reverse in terms of foldl
reverse' :: forall a. Array a -> Array a
reverse' = foldl (\acc a -> a : acc) []
