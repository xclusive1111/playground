
import qualified Data.List as L

data Cat = Cat Name CatBreed Age
type Name = String
data CatBreed =
    Siamese | Persian | Bengal | Sphynx
  | Burmese | Birman | RussianBlue
  | NorwegianForest | CornishRex | MaineCoon
type Age = Integer

data House = House HouseNumber Cat
type HouseNumber = Int

street :: [House]
street =
  [ House 1 (Cat "George" Siamese 10)
  , House 2 (Cat "Mr Bigglesworth" Persian 5)
  , House 3 (Cat "Mr Tinkles" Birman 1)
  , House 4 (Cat "Puddy" Burmese 3)
  , House 5 (Cat "Tiger" Bengal 7)
  , House 6 (Cat "The Ninja" RussianBlue 12)
  , House 7 (Cat "Mr Tinklestein"
                 NorwegianForest
                 8)
  , House 8 (Cat "Plain Cat" MaineCoon 9)
  , House 9 (Cat "Shnooby" Sphynx 7)
  , House 10 (Cat "Crazy Ears Sam"
                   CornishRex
                   3)
  ]

humanAge :: Cat -> Age
humanAge (Cat _ _ catAge)
  | catAge <= 0 = 0
  | catAge == 1 = 15
  | catAge == 2 = 25
  | otherwise   = 25 + (catAge - 2) * 4

getCatFromHouse :: House -> Cat
getCatFromHouse (House _ c) = c

getHumanAgeOfCatFromHouse :: House -> Age
getHumanAgeOfCatFromHouse =
  humanAge . getCatFromHouse

findOldestCat :: [House] -> Maybe Cat
findOldestCat []     = Nothing
findOldestCat houses = maybeOldestCat
  where
    maybeOldestCat
      = case findOldestCatHouse houses of
          Just house ->
            Just (getCatFromHouse house)
          Nothing ->
            Nothing


findOldestCatHouse :: [House] -> Maybe House
findOldestCatHouse houses =
    if length housesSortedByCatAge > 0
    then Just (head housesSortedByCatAge)
    else Nothing
  where housesSortedByCatAge
          = L.sortBy catAgeComparer houses
        catAgeComparer (House _ (Cat _ _ age1))
                       (House _ (Cat _ _ age2))
          = compare age2 age1

getCatName :: Cat -> String
getCatName (Cat name _ _) = name

getHouseNumber :: House -> String
getHouseNumber (House number _) = show number

main :: IO ()
main = putStrLn oldest
  where
    oldest =
      case findOldestCatHouse street of
        Nothing ->
          "There is no oldest cat!"
        Just house ->
          "The oldest cat is "
          ++ getCatName (getCatFromHouse house)
          ++ ", is "
          ++ show (getHumanAgeOfCatFromHouse house)
          ++ " equivalent human years old"
          ++ " and it lives in Number "
          ++ getHouseNumber house
