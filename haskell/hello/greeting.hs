-- A simple program that take a user's name and prrint it on the screen

myName = "Haskell"

message :: String -> String
message []    = "Sorry, I didn't catch your name."
message name  = ("Hello, " ++ name)

main :: IO ()
main = do
  putStrLn ("Hi there! My name is " ++ myName)
  putStrLn "What's yours ?"
  name <- getLine
  putStrLn (message name)
