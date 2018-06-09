add :: Int -> Int -> Int
add x y = x + y

plus :: Num a => a -> a -> a
plus x y = x + y

main :: IO ()
main = putStrLn (show (plus 21.5 12))
