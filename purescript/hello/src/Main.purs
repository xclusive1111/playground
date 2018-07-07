module Main where

import Prelude
import Effect (Effect)
import Effect.Console (logShow)
import Math (sqrt, pi)

main :: Effect Unit
main = do
  logShow (diagonal 3.0 4.0)

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)
