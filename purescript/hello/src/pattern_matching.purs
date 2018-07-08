module Data.Picture where

import Prelude
import Data.Foldable (foldl)
import Global as Global
import Math as Math

-- Computes the greatest common divisor of two integers
gcd :: Int -> Int -> Int
gcd n 0 = n
gcd 0 m = m
gcd n m = if n > m then gcd (n - m) m
          else gcd n (m - n)


-- Using guard
gcd' :: Int -> Int -> Int
gcd' n 0 = n
gcd' 0 m = m
gcd' n m | n > m     = gcd (n - m) m
         | otherwise = gcd n (m - n)

-- Computes factorial using pattern matching
fact :: Int -> Int
fact 0 = 1
fact 1 = 1
fact n = n * fact(n - 1)

-- Array pattern

takeFive :: Array Int -> Int
takeFive [0, 1, a, b, _] = a * b
takeFive _               = 0

squareArr :: Array (Array Int) -> Array (Array Int)
squareArr arr = map sq arr
  where
    sq []     = []
    sq [a, b] = [a * a, b * b]
    sq _      = []


-- Record pattern

-- This only work with exact match
showPerson :: { first :: String, last :: String } -> String
showPerson { first: x, last: y } = y <> ", " <> x

-- This work even if a record has additional fields
showPerson' :: forall r. { first :: String, last :: String | r } -> String
showPerson' { first: x, last: y } = y <> ", " <> x

-- Nested pattern

type Address = { street :: String, city :: String }
type Person = { name :: String, address :: Address }

livesIn :: String -> Person -> Boolean
livesIn city { address: { city: c} } = city == c
livesIn _    _                       = false


-- Named pattern

sortPair :: Array Int -> Array Int
sortPair arr@[a, b]
  | a <= b    = arr
  | otherwise = [b, a]
sortPair xs = xs

-- Case expression

squareArr' :: Array (Array Int) -> Array (Array Int)
squareArr' arr = map sq arr
  where
    sq xs = case xs of
             []     -> []
             [a, b] -> [a * a, b * b]
             _      -> []


-- Algebraic data types

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

data Point = Point
  { x :: Number
  , y :: Number
  }

showPoint :: Point -> String
showPoint (Point { x: x, y: y }) = "(" <> show x <> ", " <> show y <> ")"

showShape :: Shape -> String
showShape (Circle c r)      = showPoint c <> " " <> show r
showShape (Rectangle p h w) = showPoint p <> " " <> show h <> show w
showShape (Line p1 p2)      = showPoint p1 <> ", " <> showPoint p2
showShape (Text p s)        = showPoint p <> " " <> s

-- Record pun
showPoint' :: Point -> String
showPoint' (Point { x, y }) = "(" <> show x <> ", " <> show y <> ")"

-- Newtypes
newtype Pixels = Pixels Number
newtype Inches = Inches Number


type Picture = Array Shape

data Bounds = Bounds
  { top    :: Number
  , left   :: Number
  , bottom :: Number
  , right  :: Number
  }

showPicture :: Picture -> Array String
showPicture = map showShape
