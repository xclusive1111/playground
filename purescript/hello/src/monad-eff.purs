module Data.MonadEff where

import Prelude
import Data.List
import Data.Maybe
import Effect (Effect)
import Effect.Random (random)
import Effect.Console (logShow)
import Effect.Exception (throwException, catchException, error)
import Control.Monad.ST (ST, for, run)
import Control.Monad.ST.Ref (new, modify, read, write)


-- Folding with Monads

foldM' :: forall m a b. Monad m
      => (a -> b -> m a)
      -> a
      -> List b
      -> m a
foldM' _ a Nil      = pure a
foldM' f a (b : bs) = do
  a' <- f a b
  foldM f a' bs


filterM' :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM' f = foldM' fn Nil where 
  fn list a = do
     b <- f a
     if b
        then pure (a : list)
        else pure list


evenM n = if (n `mod` 2 == 0) 
            then Just true 
            else Just false

maybeEvenNumbers = filterM' evenM (fromFoldable [1,2,3,4,5,6])

main= do
  n <- random
  logShow n


simulate :: forall h. Number -> Number -> Int -> ST h Number
simulate x0 v0 time = do
  ref <- new { x: x0, v: v0 }
  for 0 (time * 1000) \_ -> do
     _ <- modify (\o -> { v: o.v - 9.81 * 0.001, x: o.x + o.v * 0.001 }) ref
     pure unit
  final <- read ref
  pure final.x


runSimulate :: Number -> Number -> Int -> Number
runSimulate x0 v0 time = run (simulate x0 v0 time)
