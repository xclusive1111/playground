module Data.Hash where

import Prelude
import Data.Function
import Data.Char
import Data.Foldable


newtype HashCode = HashCode Int

class Eq a <= Hashable a where
  hash :: a -> HashCode
  
instance hashInt :: Hashable Int where
  hash = hashCode

instance hashBoolean :: Hashable Boolean where
  hash false = hashCode 0
  hash true  = hashCode 1

instance hashChar :: Hashable Char where
  hash = hash <<< toCharCode

instance hashArray :: Hashable a => Hashable (Array a) where
  hash = foldl combineHashes (hashCode 0) <<< map hash

combineHashes :: HashCode -> HashCode -> HashCode
combineHashes (HashCode h1) (HashCode h2) = hashCode (73 * h1 + 51 * h2)

hashCode :: Int -> HashCode
hashCode h = HashCode (h `mod` 65535)

