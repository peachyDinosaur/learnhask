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
findOldestCat houses =
     Just oldestCat
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

--CHAPTER 15
-- very weird chapter


-- with type def
helloString :: String
helloString = "Hello"

hello :: IO ()
hello = putStrLn("Hello")

n1 :: Int
n1 = 1

n2 :: Int
n2 = 1

sumNums :: Int -> Int -> Int
sumNums x y = x + y

-- or
theAnswerNumber :: Integer
theAnswerNumber = 3000+ 2000

-- main :: IO ()
-- main = print theAnswerNumber

productOfInt :: Integer
productOfInt = 33*398

-- main :: IO ()
-- main = print productOfInt

sentence :: String
sentence = "This sentence is False"

otherSentence :: String
otherSentence = "No it's not"

addMonthToYear :: Int -> Int
addMonthToYear x = 2020+x

addNumbers :: Int
addNumbers = 5+7+8+9


-- main :: IO ()
-- main = print addNumbers

-- CHAPTER 16

main :: IO ()
main = do
    putStrLn "You are in the fridge. What do you want to do ?"
    putStrLn "1. Try to get out"
    putStrLn "2. Eat."
    putStrLn "3. Die."
    command <- getLine
    case command of
        "1" ->
            putStrLn "You try to get out. You fail you die."
        "2" ->
            putStrLn "You eat too much food. You die"
        "3" ->
            putStrLn "You die"
        _ ->
            putStrLn "Did not understand. So you die"
    putStrLn "Play again? write y for yes"
    playAgain <- getLine
    if playAgain == "y"
    then main
    else putStrLn "Thanks for playing"

--CHAPTER 17

--type Name = String
type Year = Int
-- data Person = Person Name Name Year
--     deriving (Show)

--type Name = String
--type Year = Int




data Person = Person 
     { personFirstName :: Name
     , personLastName :: Name
     , yearOfBirth:: Year}
    deriving (Show)


blaise :: Person
blaise = Person "Blaise" "Pascal" 1623

blaise' :: Person
blaise' =      
    Person { personFirstName = "Blaise"
            , personLastName = "Pascal"
            , yearOfBirth = 1623 }

traise :: Person
traise =  blaise' 
    { personFirstName = "Traise"}

people :: [Person]
people =
  [ Person "Isaac" "Newton" 1643
  , Person "Leonard" "Euler" 1707
  , Person "Blaise" "Pascal" 1623
  , Person "Ada" "Lovelace" 1815
  , Person "Alan" "Turing" 1912
  , Person "Haskell" "Curry" 1900
  , Person "John" "von Neumann" 1903
  , Person "Lipot" "Fejer" 1880
  , Person "Grace" "Hopper" 1906
  , Person "Anita" "Borg" 1949
  , Person "Karen" "Sparck Jones" 1935
  , Person "Henriette" "Avram" 1919 ]


firstAfter1900 :: Maybe Person
firstAfter1900 = 
    L.find(\(Person _ _ year) -> year == 1900) people


firstNameBeginsWithL :: Person -> Bool
firstNameBeginsWithL p = 
    case personFirstName p of 
        'L':_ -> True
        _     -> False

makeNewListWithOnlyLPeople :: [Person] -> [Person]
makeNewListWithOnlyLPeople [] = []
makeNewListWithOnlyLPeople (x:xs)
    | firstNameBeginsWithL x = 
       x : makeNewListWithOnlyLPeople xs
    | otherwise =
        makeNewListWithOnlyLPeople xs

makeNewListWithOnlyLPeople' :: [Person] -> [Person]
makeNewListWithOnlyLPeople' xs = 
    filter firstNameBeginsWithL xs




firstLetterIs :: Char -> String -> Bool
firstLetterIs c "" = False
firstLetterIs c (x:_) = c == x

firstNameBeginsWith :: Char -> Person -> Bool
firstNameBeginsWith c p =
    firstLetterIs c firstName
    where firstName = personFirstName p

peopleThatBeginWithL'' :: [Person]
peopleThatBeginWithL'' =
    filter (firstNameBeginsWith 'K') people

peopleThatBeginWithL = makeNewListWithOnlyLPeople' people

mapPeople :: (Person -> String) -> [Person] -> [String]
mapPeople f [] = []
mapPeople f (x:xs) = 
    f x : mapPeople f xs

-- peopleToFirstNames :: [Person] -> [String]
-- peopleToFirstNames people =
--     mapPeople personFirstName people

peopleToFirstNames :: [Person] -> [String]
peopleToFirstNames = mapPeople personFirstName

firstNames :: [String]
firstNames = map personFirstName people


sortedFirstName :: [String]
sortedFirstName = L.sort firstNames

sortedFirstName' :: [String]
sortedFirstName' = reverse(L.sort firstNames)

sortedFirstName'' :: [String]
sortedFirstName'' = L.sortBy reverseCompare firstNames
    where reverseCompare = flip compare

yearsSinceBirthAtYear :: Year -> Person -> Int
yearsSinceBirthAtYear y p = y - yearOfBirth p

allYearsSinceBirth :: [Int]
allYearsSinceBirth = 
    map (yearsSinceBirthAtYear 2020) people

earliestYearOfBirth :: [Person] -> Int
earliestYearOfBirth people =
    minimum (L.map yearOfBirth people)

bornFirst :: [Person] -> Person
bornFirst people =
    L.minimumBy compareBirthYears people
  where compareBirthYears x y =
          compare (yearOfBirth x) (yearOfBirth y)
        

-- CHAPTER 18

type Level = (Integer, Integer)

levels :: [Level]
levels =
    concat $ map pairForNums [3,5..12]
    where
        pairForNums num = zip[2..12] $ repeat num 

levelNumber :: [a] -> Int
levelNumber remainingLevels = 
    totalLevels - levelsLeft
  where totalLevels = length levels + 1
        levelsLeft  = length remainingLevels
    


main2 :: IO ()
main2 = do
  putStrLn "Suddenly, you wake up. Oh no, you're on..."
  putStrLn "The Times-Table Train of Terror!"
  putStrLn "Try to get to the end. We DARE you!"
  trainLoop levels

trainLoop :: [Level] -> IO()
trainLoop [] = 
    putStrLn "you beat the game"
trainLoop remainingLevels @ (currentLevel : levelsAfterThisOne) =
    do
        let currentLevelNumber = 
                levelNumber remainingLevels
            (num1, num2) =
                currentLevel
        putStrLn $
            "You are in a Train Carraige "
            ++ show currentLevelNumber
            ++ " of " ++ (show $ length levels)
        putStrLn "Do you want to:"
        putStrLn "1. Go to next carraige"
        putStrLn "2. Jump out of the train"
        putStrLn "3. Eat some food"
        putStrLn "q Quit"
        activity <- getLine
        case activity of
            "1" ->
                do
                    putStrLn $ "You try to go to the next carriage."
                             ++"The Door is locked"
                    putStrLn "Answer this question to open the door"
                    putStrLn $ "What is " ++ show num1
                             ++ " times " ++ show num2 ++ "?"
                    answer <- getLine
                    if answer == (show $ num1 * num2)
                    then do
                        putStrLn "The door is open now"
    
                        trainLoop levelsAfterThisOne
                    else do
                        putStrLn $ "Wrong. You try to open the lock,"
                                ++ " but it won't open."
                        trainLoop remainingLevels
            "2" -> jumpingFutility
            "3" -> eatingFutility
            "q" -> putStrLn $ "You decide to quit."
                            ++ " Thanks for playing. Bah-Bye!"
            _   -> do
                putStrLn "That makes NO sense! Try again."
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

printBirthday :: IO()
printBirthday = do
    putStrLn "what year were you born"
    answer <- getLine
    putStrLn $ "so your birthday is "++ answer

-- CHAPTER 19
-- Really convoluted exercise/chp

data GameObject = Player
                | Acorn
  deriving (Eq, Show)
data Room =
  Room Description [GameObject]
    deriving (Show)
type Description = String
type Inventory = [GameObject]
type GameMap = [Room]
type GameState = (GameMap, Inventory)


initialState :: GameState
initialState =
  ( [ Room "You are inside a tree." [Player]
    , Room "You are outside of a tree." [Acorn]]
  , [] )

main3 :: IO ()
main3 = do
  putStrLn "Welcome to Skwak the Squirrel."
  putStrLn "You are a squirrel."
  gameLoop initialState

gameLoop :: GameState -> IO ()
gameLoop (rooms, currentInv) = do
  let currentRoom =
        case findRoomWithPlayer rooms of
        Just r -> r
        Nothing -> error $ "Somehow the player "
                         ++ "ended up outside the map!"
      possibleCmds =
        validCommands currentRoom currentInv
  if playerWon (rooms, currentInv)
  then gameOverRestart
  else do
    describeWorld currentRoom currentInv possibleCmds
    takeActionThenLoop
      currentRoom currentInv possibleCmds rooms

findRoomWithPlayer :: [Room] -> Maybe Room
findRoomWithPlayer rooms =
  L.find (\(Room _ obs) ->
            any (== Player) obs)
         rooms

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
somethingToTake objs =
  any (/= Player) objs


playerWon :: GameState -> Bool
playerWon (rooms, currentInv) =
    any hasAcornAndInside rooms
  where hasAcornAndInside (Room desc objs) =
          desc == "You are inside a tree."
          && any (==Acorn) objs


gameOverRestart :: IO ()
gameOverRestart = do
  putStrLn $ "You won!"
    ++ "You have successfully stored the acorn"
    ++ " for winter. Well done!"
  putStrLn "Do you want to play again? y = yes"
  playAgain <- getLine
  if playAgain == "y"
  then gameLoop initialState
  else putStrLn "Thanks for playing!"

getCommand :: IO String
getCommand = do
  putStrLn "What do you want to do?"
  getLine

takeActionThenLoop :: Room ->
                      Inventory ->
                      [String] ->
                      [Room] ->
                      IO ()
takeActionThenLoop currentRoom
                   currentInv
                   possibleCmds
                   rooms =
  do
    command <- getCommand
    if any (==command) possibleCmds
    then case command of
      "go" ->
          do
            putStrLn "You go..."
            gameLoop $ movePlayer (rooms, currentInv)
      "take" ->
          do
            putStrLn "You take the acorn..."
            gameLoop $
              moveAcornToInventory (rooms, currentInv)
      "put" ->
          do
            putStrLn "You put the acorn down..."
            gameLoop $
              moveAcornFromInventory (rooms, currentInv)
      "quit" ->
          putStrLn $ "You decide to give up."
                   ++ " Thanks for playing."
      _ ->
          do
            putStrLn "That is not a command."
            gameLoop (rooms, currentInv)
    else do
      putStrLn $ "Command not possible here,"
               ++ " or that is not a command."
      gameLoop (rooms, currentInv)

movePlayer :: GameState -> GameState
movePlayer (rooms, inv) =
    (newRooms, inv)
  where
    newRooms =
      map adjustRooms rooms
    adjustRooms (Room d objs) =
      if any (==Player) objs
      then (Room d (filter (/=Player) objs))
      else (Room d (Player : objs))

moveAcornToInventory :: GameState -> GameState
moveAcornToInventory (rooms, inv) =
    (newRooms, newInv)
  where
    newInv =
      Acorn : inv
    newRooms =
      map adjustRooms rooms
    adjustRooms (Room d objs) =
      Room d (filter (/=Acorn) objs)

moveAcornFromInventory :: GameState -> GameState
moveAcornFromInventory (rooms, inv) =
    (newRooms, newInv)
  where
    newInv =
      filter (/=Acorn) inv
    newRooms =
      map adjustRooms rooms
    adjustRooms (Room d objs) =
      if any (==Player) objs
      then Room d (Acorn : objs)
      else Room d objs

describeWorld :: Room ->
                 Inventory ->
                 [String] ->
                 IO ()

describeWorld currentRoom
              currentInv
              possibleCmds =
  do
    putStrLn $ describeRoom currentRoom
    putStrLn $ describeInventory currentInv
    putStrLn $ describeCommands possibleCmds


describeRoom :: Room -> String
describeRoom (Room desc objs) =
  desc ++ if any (==Acorn) objs
          then " There is an acorn here"
          else ""

describeInventory :: Inventory -> String
describeInventory [] =
  "You are holding nothing"
describeInventory inv =
  "You are holding: " ++
  (concat $ map show inv)

describeCommands :: [String] -> String
describeCommands commands =
  "Commands: "
  ++ (L.intercalate ", " commands)

