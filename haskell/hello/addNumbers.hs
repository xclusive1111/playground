-- Ask two numbers from user and add them together

add :: Num a => a -> a -> a
add a b = a + b

main :: IO ()
main = do
  putStrLn "Enter your first number:"
  a <- getLine
  putStrLn "Enter your second number:"
  b <- getLine
  putStrLn ("Results: " ++ (show (add (read a :: Int) (read b :: Int))))
  putStrLn "Continue? (Y/N)"
  command <- getLine
  if command == "Y"
  then main
  else putStrLn "Thanks!"
