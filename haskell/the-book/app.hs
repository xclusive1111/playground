-- A simple program that builds a list of favorite people
-- from the subject areas of maths and computing
-- We also experiment with higher order function, such as:
-- filter, map, sortBy ...

import qualified Data.List as L

type Name = String
type Year = Int
-- value constructor:
-- Person :: Name -> Name -> Year -> Person
data Person = Person
  { personFirstName :: Name
  , personLastName :: Name
  , yearOfBirth :: Year }
  deriving (Show)

-- The famous mathematican
-- Blaise Pascal
blaise :: Person
blaise = Person
  { personFirstName  = "Blaise"
  , personLastName   = "Pascal"
  , yearOfBirth      = 1623
  }

-- record update syntax
traise :: Person
traise = blaise { personFirstName = "Traise" }

people :: [Person]
people = 
  [ Person "Isaac" "Newton" 1643
  , Person "Leonard" "Euler" 1707
  , Person "Blaise" "Pascal" 1623
  , Person "Ada" "Lovelace" 1815
  , Person "Haskell" "Curry" 1900
  , Person "Lipot" "Fejer" 1883
  , Person "Karen" "Sparck Jones" 1935
  ]

-- finding a person from the list
-- using pattern match
firstAfter1900 :: Maybe Person
firstAfter1900 = L.find (\(Person _ _ year) -> year >= 1900) people
-- using getter
firstAfter1900' :: Maybe Person
firstAfter1900' = L.find(\person -> yearOfBirth person >= 1900) people

-- filtering out persons from the list
firstNameBeginWithL :: Person -> Bool
firstNameBeginWithL p =
  case personFirstName p of
    'L':_ -> True
    _     -> False

makeNewListWithOnlyLPeople :: [Person] -> [Person]
makeNewListWithOnlyLPeople []     = []
makeNewListWithOnlyLPeople (x:xs)
    | firstNameBeginWithL x = x : makeNewListWithOnlyLPeople xs
    | otherwise = makeNewListWithOnlyLPeople xs

makeNewListWithOnlyLPeople' :: [Person] -> [Person]
makeNewListWithOnlyLPeople' xs = filter firstNameBeginWithL xs
-- Eta reduction, i.e: 'xs' get erased
makeNewListWithOnlyLPeople'' :: [Person] -> [Person]
makeNewListWithOnlyLPeople'' = filter firstNameBeginWithL

firstLetterIs :: Char -> String -> Bool
firstLetterIs c "" = False
firstLetterIs c (x:_) = c == x

firstNameBeginWith :: Char -> Person -> Bool
firstNameBeginWith c (Person (x:_) _ _) = c == x

peopleThatBeginWithL :: [Person]
peopleThatBeginWithL = filter (firstNameBeginWith 'L') people

mapPeople :: (Person -> String) -> [Person] -> [String]
mapPeople f []     = []
mapPeople f (x:xs) = f(x) : mapPeople f xs

-- with Eta reduction
peopleToLastNames :: [Person] -> [String]
peopleToLastNames = mapPeople personLastName 

-- using map
lastNames :: [String]
lastNames = map personLastName people

firstNames :: [String]
firstNames = map personFirstName people


sortedLastNames :: [String]
sortedLastNames = L.sort lastNames

reverseSortedLastNames :: [String]
reverseSortedLastNames = L.sortBy (\x y -> compare y x) lastNames

-- takes a year, and a Person and works out how many
-- years ago from that year that person was born
yearsSinceBirthAtYear :: Year -> Person -> Int
yearsSinceBirthAtYear y (Person _ _ born) = y - born

-- get years since birth across all the people
allYearsSinceBirthAt2018 :: [Int]
allYearsSinceBirthAt2018 = map (yearsSinceBirthAtYear 2018) people

-- get the person that has the earliest year of birth
-- using built-in minimum function:
-- minimum :: (Ord a, Foldable t) => t a -> a
earliestYearOfBirth :: [Person] -> Year
earliestYearOfBirth people = minimum (L.map yearOfBirth people)
-- removing parentheses with ($) function
earliestYearOfBirth' :: [Person] -> Year
earliestYearOfBirth' people = minimum $ L.map yearOfBirth people

-- find out which people was born first
-- using minimumBy:
-- minimumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
bornFirst :: [Person] -> Person
bornFirst people = L.minimumBy compareBirthYears people
  where compareBirthYears p1 p2 = compare (yearOfBirth p1) (yearOfBirth p2) 

main :: IO ()
main = putStrLn "Ok"
