import System.Environment (getArgs)

fixLines :: String -> String
fixLines input = unlines (splitLines input)

-- Split lines of text
splitLines []  = []
splitLines xs  = 
  let (pre, suf) = break isLineTerminator xs
  in   pre : case suf of
              ('\r':'\n':rest) -> splitLines rest
              ('\n': rest)     -> splitLines rest
              ('\r': rest)     -> splitLines rest
              _                -> []

isLineTerminator c = c == '\r' || c == '\n'

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input, output] -> interactWith function input output
            _               -> putStrLn "error: exactly teo arguments needed"
        myFunction = fixLines

