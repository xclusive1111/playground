module Data.MonadEff where

import Prelude
import Data.List
import Data.Maybe
import Control.MonadZero (guard)


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
