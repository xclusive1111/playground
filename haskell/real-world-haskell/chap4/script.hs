-- Computes the number of elements in a list
lengthL :: [a] -> Int
lengthL []     = 0
lengthL (_:xs) = 1 + lengthL xs

-- more generic
lengthL' :: Foldable t => t a -> Int
lengthL' = foldr inc 0
  where inc _ b = 1 + b

-- using lambda
lengthL'' :: Foldable t => t a -> Int
lengthL'' = foldr (\_ b -> 1 + b) 0

-- computes the sum of a list of number
sumL :: (Num a, Foldable t) => t a -> a
sumL = foldr (\a b -> a + b) 0

-- computes the mean of a list of numbers
mean :: Fractional a => [a] -> a
mean xs = sumL xs / fromIntegral (lengthL xs)

-- e.g: [1,2,3] -> [1,2,3,3,2,1]
palindrome :: [a] -> [a]
