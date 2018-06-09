-- Simple program to display the sum of 2 numbers

add :: Num a => a -> a -> a
add a b = a + b

main :: IO ()
main = putStrLn (show (add 28 49.5))
