import qualified Data.List as L


-- CHAPTER 7

-- If clause

message' :: String -> String
message' name = if name == "Dave"
    then "I can't do that dave"
    else if name == "Sam"
        then "play it again"
    else "Hello"

-- Case expression

message'' :: String -> String
message'' name = 
    case name of
        "Dave" -> "I can't do that"
        "Sam" -> "play it again"
        _ -> "hi"

-- Guard Pattern

message :: String -> String
message name 
    | name == "Dave" = "i can't do that"
    | name == "Sam" = "play it again"
    | otherwise     = "hi"

addThem :: Int -> Int -> Int ->Int -> Int
addThem a b c d = a+b+c+d

addThem2 :: Int -> Int -> Int ->Int
addThem2 = addThem 1

addThem3 :: Int -> Int -> Int
addThem3 = addThem2 2


-- CHAPTER 8

aList = []

aList2 = "sauce" : aList

aList3  :: [String]
aList3 = ["napkin"]

aList4 :: [] String
aList4 = ["other napkin"]

aList5 :: [Integer]
aList5 = [2]

aList6 :: [Integer]
aList6 = 533 : []

aList7 :: Num a => [a]
aList7 = [1, 3]

-- shoppingList :: [String]
-- shoppingList = 
--     ["Carrots"
--     ,"oats"
--     ,"soy milk"
--     ,"rice"
--     ,"tofu"
--     ]

-- main ::IO ()
-- main = putStrLn ("there are "
--                 ++(show (length(shoppingList)))
--                 ++ " items on the shopping list"
--                 ++ " and the first item is: "
--                 ++ firstOrEmpty shoppingList
--                 ++ " The entire list is: "
--                 ++ joinedWithCommas shoppingList)

firstOrEmpty :: [String] -> String
firstOrEmpty [] = ""
firstOrEmpty (x:_) = x

lastOrEmpty :: [String] -> String
lastOrEmpty [] = ""
lastOrEmpty (x:y) = 
    if y == []
    then x
    else lastOrEmpty y

firstTwoOrEmpty :: [String] -> String
firstTwoOrEmpty [] = ""
firstTwoOrEmpty [x] = x
firstTwoOrEmpty (x:y:_) = x ++ ", " ++ y

joinedWithCommas :: [String] -> String
joinedWithCommas [] = ""
joinedWithCommas [x] = x
joinedWithCommas (x:xs) = x ++ " , " ++ joinedWithCommas xs


-- CHAPTER 10


movies =
  [ "Aeon Flux"
  , "The Black Cat"
  , "Superman"
  , "Stick It"
  , "The Matrix Revolutions"
  , "The Raven"
  , "Inception"
  , "Looper"
  , "Hoodwinked"
  , "Tell-Tale"
  ]

isGood :: String -> Bool
isGood (x:_) = x <= 'B'
isGood _ = False

asses :: String -> String
asses movie = movie ++ " - " ++ assesment
    where assesment = if isGood movie
                      then "Good"
                      else "Bad"


assesMovie :: [String] -> [String]
assesMovie [] = []
assesMovie(x:xs) = asses x : assesMovie xs

assesedMovie :: [String]
assesedMovie = assesMovie movies

-- main :: IO ()
-- main = putStrLn (L.intercalate "\n" assesedMovie)

--CHAPTER 11

-- aShoppingListItem :: (String, Int)
-- aShoppingListItem = ["Bananas", 300]

-- Type Alias
-- type ShoppingListItem =(String, Int)

-- aShoppingListItem :: ShoppingListItem
-- aShoppingListItem = ["Bananas", 300]

type Item  = String
type PriceInCents = Int
type ShoppingListItem = (Item, PriceInCents)
type ShoppingList = [ShoppingListItem]

shoppingList :: ShoppingList
shoppingList = [ ("Bananas", 300)
               , ("Chocolate", 250)
               , ("Milk", 300)
               , ("Apples", 650)
               ]

sumShoppingList :: ShoppingList -> PriceInCents
sumShoppingList [] = 0
sumShoppingList (x:xs) = getPriceFromItem x + 
                         sumShoppingList xs

sumShoppingList' :: ShoppingList -> PriceInCents
sumShoppingList' xs = 
    foldr getPriceAndAdd 0 xs

getPriceAndAdd :: ShoppingListItem -> 
                  PriceInCents -> 
                  PriceInCents
getPriceAndAdd item currentTotal = 
    getPriceFromItem item + currentTotal

getPriceFromItem :: ShoppingListItem -> PriceInCents
getPriceFromItem (_, price) = price

-- main :: IO ()
-- main = putStrLn ("price of shopping list is "
--                 ++ show (sumShoppingList' shoppingList)
--                 ++ " cents")

-- CHAPTER 13
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

adviceOnEscape :: Animal -> String
adviceOnEscape animal =
    case animal of
        Giraffe -> "Look up"
        Elephant -> "Ear to the ground"
        Tiger -> "Check the morgues"
        Flea -> "Don't worry"

adviceOnZooEscape :: Zoo -> [String]
adviceOnZooEscape [] = []
adviceOnZooEscape (x:xs) = 
    adviceOnEscape x : adviceOnZooEscape xs

adviceOnZooEscape' :: Zoo -> [String]
adviceOnZooEscape' xs = map adviceOnEscape xs
-- drop xs
-- adviceOnZooEscape' = map adviceOnEscape

-- main :: IO ()
-- main = putStrLn stringToPrint
--     where
--         -- Using joinedWithCommas func
--         -- stringToPrint  = joinedWithCommas advices 
--         stringToPrint  = L.intercalate " , " advices
--         advices = adviceOnZooEscape' localZoo


type Name = String
type Greeting = String

printName :: Name -> Greeting
printName "" = "Hello There"
printName name = "Hello " ++ name

-- CHAPTER 14

data CatBreed = 
    Siamese | Persian | Bengal | Sphynx
  | Burmese | Birman | RussianBlue
  | NorwegianForest | CornishRex | MaineCoon
  deriving Show

type CatName = String
type Age = Integer
data Cat = Cat CatName CatBreed Age deriving Show

type HouseNumber = Int
data House = House HouseNumber Cat

humanAge :: Cat -> Age
humanAge (Cat _ _ catAge)
    | catAge <= 0 = 0
    | catAge == 1 = 15
    | catAge == 2 = 25
    | otherwise = 25 + (catAge-2) * 4

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

getCatFromHouse :: House -> Cat
getCatFromHouse (House _ c) = c

getHumanAgeOfCatFromHouse :: House -> Age
getHumanAgeOfCatFromHouse = 
    humanAge . getCatFromHouse

findOldestCat :: [House] -> Maybe Cat
findOldestCat [] = Nothing
findOldestCat houses = Just oldestCat
    where
        oldestCat
         = getCatFromHouse houseWithOldestCat
        houseWithOldestCat
         = head housesSortedByCatAge
        housesSortedByCatAge
         = L.sortBy catAgeComparer houses
        catAgeComparer (House _ (Cat _ _ age1))
                       (House _ (Cat _ _ age2))
         = compare age2 age1 
