-- A simple program that display the prod of two numbers

prod :: Num a => a -> a -> a
prod a b = a * b

main :: IO ()
main = print (show (prod 5 12.5))
