import qualified Data.List as L
-- Sum type
data Animal = Giraffe
            | Elephant
            | Tiger
            | Flea

type Zoo = [Animal]
localZoo :: Zoo
localZoo = [ Elephant
           , Tiger
           , Tiger
           , Giraffe
           , Elephant
           ]

-- Use case expression
adviceOnEscape :: Animal -> String
adviceOnEscape animal =
  case animal of
    Giraffe  -> "Look up"
    Elephant -> "Ear to the ground"
    Tiger    -> "Check to morgues"
    Flea     -> "Don't worry"

-- Use recursion
adviceOnZooEscape :: Zoo -> [String]
adviceOnZooEscape []     = []
adviceOnZooEscape (x:xs) = adviceOnEscape x : adviceOnZooEscape xs

-- map :: (a -> b) -> [a] -> [b]
adviceOnZooEscape' :: Zoo -> [String]
adviceOnZooEscape' xs = map adviceOnEscape xs

adviceOnZooEscape'' :: Zoo -> [String]
adviceOnZooEscape'' = map adviceOnEscape

main :: IO ()
main = putStrLn stringToPrint
  where stringToPrint = L.intercalate ", " advices
        advices = adviceOnZooEscape localZoo
