import qualified Data.List as L

data GameObject = Player | Acorn
  deriving (Eq, Show)
data Room = Room Description [GameObject]
  deriving (Show)
type Description = String
type Inventory = [GameObject]
type GameMap = [Room]
type GameState = (GameMap, Inventory)

initialState :: GameState
initialState = ([ Room "You are inside a tree." [Player]
                , Room "You are outside of a tree."  [Acorn]
                ], [])

main :: IO ()
main = do
  putStrLn "Welcome to Skwak the Squirrel."
  putStrLn "You are a squirrel."
  gameLoop initialState

gameLoop :: GameState -> IO ()
gameLoop (rooms, currentInv) = do
  let currentRoom = 
        case findRoomWithPlayer rooms of
          Just r -> r
          Nothing -> error $ "Somehow the player ended up outside the map!"
      possibleCmds = validCommands currentRoom currentInv
  if playerWon (rooms, currentInv)
  then gameOverRestart
  else do
    describeWorld currentRoom currentInv possibleCmds
    takeActionThenLoop
      currentRoom currentInv possibleCmds rooms

findRoomWithPlayer :: [Room] -> Maybe Room
findRoomWithPlayer rooms = L.find (\(Room _ obs) -> any (== Player) obs) rooms

validCommands :: Room -> Inventory -> [String]
validCommands (Room _ gameObjs) invItems = 
  ["go"] ++ takeCommandList
  ++ dropCommandList ++ ["quit"]
  where
    takeCommandList =
      if somethingToTake gameObjs
      then ["take"]
      else []
    dropCommandList =
      if length invItems > 0
      then ["put"]
      else []
    
somethingToTake :: [GameObject] -> Bool
somethingToTake  = any (/= Player) 

playerWon :: GameState -> Bool
playerWon (rooms, currentInv) = any hasAcornAndInside rooms
    where hasAcornAndInside (Room desc objs) = desc == "You are inside a tree." && any (== Acorn) objs


gameOverRestart :: IO ()
gameOverRestart = do
  putStrLn $ "You won!"
    ++ "You have successfully stored the acorn"
    ++ " for winter. Well done!"
  putStrLn "Do you want to play again? y = yes"
  cmd <- getLine
  if cmd == "y"
  then gameLoop initialState
  else putStrLn "Thanks for playing!"

getCommand :: IO String
getCommand = do
  putStrLn "What do you want to do?"
  getLine

takeActionThenLoop :: Room -> Inventory -> [String] -> [Room] -> IO ()
takeActionThenLoop currentRoom
                   currentInv
                   possibleCmds
                   rooms =
    do
      command <- getCommand
      if any (== command) possibleCmds
      then case command of
             "go"   -> do
               putStrLn "You go..."
               gameLoop $ movePlayer (rooms, currentInv)
             "take" -> do
               putStrLn "You take the acorn..."
               gameLoop $ moveAcornToInventory (rooms, currentInv)
             "put"  -> do
               putStrLn "You put the acorn down..."
               gameLoop $ moveAcornFromInventory (rooms, currentInv)
             "quit" -> do
               putStrLn $ "You decide to give up. " ++ "Thanks for playing."
             _      -> do
               putStrLn "That is not a command."
               gameLoop (rooms, currentInv)
      else do
        putStrLn $ "Command not possible here, or that is not a command."
        gameLoop (rooms, currentInv)

movePlayer :: GameState -> GameState
movePlayer (rooms, inv) = (newRooms, inv)
  where 
    newRooms = map adjustRooms rooms
    adjustRooms (Room d objs) =
      if any (== Player) objs
      then (Room d (filter (/= Player) objs))
      else (Room d (Player : objs))

moveAcornToInventory :: GameState -> GameState
moveAcornToInventory (rooms, inv) = (newRooms, newInv)
    where
      newInv = Acorn : inv
      newRooms = map adjustRooms rooms
      adjustRooms (Room d objs) = Room d (filter (/= Acorn) objs)

moveAcornFromInventory :: GameState -> GameState
moveAcornFromInventory (rooms, inv) = (newRooms, newInv)
    where
      newInv = filter (/= Acorn) inv
      newRooms = map adjustRooms rooms
      adjustRooms (Room d objs) =
        if any (== Player) objs
        then Room d (Acorn : objs)
        else Room d objs

describeWorld :: Room -> Inventory -> [String] -> IO ()
describeWorld currentRoom currentInv possibleCmds = do
  putStrLn $ describeRoom currentRoom
  putStrLn $ describeInventory currentInv
  putStrLn $ describeCommands possibleCmds

describeRoom :: Room -> String
describeRoom (Room desc objs) =
  desc ++ if any (== Acorn) objs
          then " There is an acorn here"
          else ""

describeInventory :: Inventory -> String
describeInventory []  = "You are holding nothing"
describeInventory inv = "You are holding: " ++ (concat $ map show inv)

describeCommands :: [String] -> String
describeCommands commands = "Commands: " ++ (L.intercalate ", " commands)
