-- Introduce a few new functions and features of Haskell

type Level = (Integer, Integer)
levels :: [Level]
levels = concat $ map pairsForNum [3,5..12]
  where pairsForNum num = zip [2..12] $ repeat num

levelNumber :: [a] -> Int
levelNumber remainingLevels = totalLevels - levelsLeft
  where totalLevels = length levels + 1
        levelsLeft  = length remainingLevels

main :: IO ()
main = do
  putStrLn "Suddenly, you wake up. Oh no, you're on..."
  putStrLn "The Times-Table Train of Terror!"
  putStrLn "Try to get the end. We DARE you!"
  trainLoop levels


trainLoop :: [Level] -> IO ()
trainLoop [] = putStrLn "You won! Well done."
trainLoop remainingLevels @ (currentLevel : levelsAfterThisOne) = do
  let currentLevelNumber = levelNumber remainingLevels
      (num1, num2)       = currentLevel
  putStrLn $ "You are in a Train Carriage "
           ++ show currentLevelNumber
           ++ " of " ++ (show $ length levels)
  putStrLn "Do you want to:"
  putStrLn "1. Go to the next Carriage"
  putStrLn "2. Jump out of the train"
  putStrLn "3. Eat some food"
  putStrLn "q. Quit"
  activity <- getLine
  case activity of
    "1" -> do
      putStrLn $ "You try to go to the next carriage."
               ++ " The door is locked."
      putStrLn "Answer this question to unlock the door:"
      putStrLn $ "What is " ++ show num1 ++ " times " ++ show num2 ++ "?"
      answer <- getLine
      if answer == (show $ num1 * num2)
      then do
        putStrLn "The lock is opened"
        trainLoop levelsAfterThisOne
      else do
        putStrLn $ "Wrong. You try to open the lock, but it won't open"
        trainLoop remainingLevels
    "2" -> jumpingFutility
    "3" -> eatingFutility
    "4" -> putStrLn $ "You decide to quit. Thanks for playing!"
    _   -> do
      putStrLn "That makes NO sense! Try again"
      trainLoop remainingLevels
    
jumpingFutility :: IO ()
jumpingFutility = do
  putStrLn "You try to jump out of the train."
  putStrLn "You fail and die."
  trainLoop levels

eatingFutility :: IO ()
eatingFutility = do
  putStrLn "You see a delicious looking cupcake."
  putStrLn "You eat it. It's a time travel cupcake!"
  trainLoop levels
