doubleMe x = x+x

doubleUs x y = doubleMe x+ doubleMe y

doubleSmallNum x = if x > 100 then x else x*2
doubleSmallNum' x = (if x > 100 then x else x *2)+1

lostNums = [2,4,6,7]

elias = "elias"
myNameIs name = "my name is " ++ name

boomBangs xs = [if x < 10 then "Boom!" else "Bang!" | x <- xs, odd x]

nouns = ["hobo", "frog", "pope"]
adj = ["lazy", " grouncy", "scheming"]

lenght' xs = sum[1 | _ <- xs]

removeUppercase :: [Char] -> [Char] 
removeUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

xxs = [[1..4],[4..8],[10..16]]

getEvens xxs = [[x | x <- xs, even x] | xs <- xxs]

triples = [ (a,b,c) | c <- [1..10], a <- [1..c], b <- [1..a], a^2 + b^2 == c^2, a+b+c ==24 ]

addThree :: Int->Int->Int->Int
addThree x y z = x+y+z

-- factorial :: Integer-> Integer
-- factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r


lucky :: Int -> String
lucky 7 = "Lucky number seven"
lucky x = "sorry youre not lucky"

saysMe :: Int -> String
saysMe 1 = "One"
saysMe 2 = "two"
saysMe 3 = "three"
saysMe 4 = "four"
saysMe _ = "not between ..."


factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n -1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Bart"

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1+x2, y1+y2)

first :: (a,b,c) -> a
first (x, _, _) = x

second :: (a,b,c) -> b
second (_, y, _) = y

third :: (a,b,c) -> c
third (_, _, z) = z

xs = [(1,3),(4,3),(2,4),(5,3),(5,6),(6,1)]

head' :: [a] -> a
head' [] = error "can't call empty list"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "list empty"
tell (x:[]) = "this list has one element: " ++ show x
tell (x:y:[]) = "this list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_:[]) = "this list is long. The first two elements are: " ++ show x ++ " and " ++ show y

badAdd :: (Num a) => [a] ->a
badAdd (x:y:z:[]) = x + y + z

firstLetter :: String -> String
firstLetter "" = "empty string"
firstLetter all@(x:xs) = "the firstLetter in " ++ all ++ " is " ++ [x]

bmiTell :: Double -> String
bmiTell bmi
   | bmi <= 18.5 = "youre underweight"
   | bmi <= 25.0 = "youre normal"
   | bmi <= 30.0 = "youre overweight"
   | otherwise = "youre obese"


bmiTell' :: Double -> Double -> String
bmiTell' weight height
   | weight/height ^2 <= 18.5 = "youre underweight eat more"
   | weight/height ^2 <= 25.0 = "youre looking good"
   | weight/height ^2 <= 30.0 = "youre overweight workout a bit"
   | otherwise = "see doctor"

max' :: (Ord a) => a -> a -> a 
max' a b
   | a <= b = b
   | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
   | a == b    = EQ
   | a <= b    = LT
   | otherwise = GT

bmiTell'' :: Double -> Double -> String
bmiTell'' weight height
   | bmi <= 18.5 = "youre underweight eat more"
   | bmi <= 25.0 = "youre looking good"
   | bmi <= 30.0 = "youre overweight workout a bit"
   | otherwise = "see doctor"
   where bmi = weight/height ^2

niceGreeting :: String
niceGreeting = " Hello! Nice to see you "

badGreeting :: String
badGreeting = "Oh ! It's you "

great :: String -> String
great "Juan" = niceGreeting ++ "Juan!"
great "Fernando" = niceGreeting ++ "Fernando"
great name = badGreeting ++ name

initals :: String -> String -> String
initals firstName secondName = [f] ++ ". " ++ [l] ++ "."
    where (f: _) = firstName
          (l: _) = secondName

calBmi :: [(Double, Double)] -> [Double]
calBmi xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight/height ^ 2

cylinder :: Double -> Double -> Double
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea

calBmi' :: [(Double, Double)] -> [Double]
calBmi' xs = [bmi | (w, h) <- xs, let bmi = w/h ^2, bmi >25.0]

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "list empty"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: Int -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x : replicate' (n-1) x

take' :: Int -> [a] -> [a]
take' n _ 
    | n <= 0   = []
take' _ []     = []
take' n (x:xs) =  x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x: repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y): zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs) 
     | a == x = True
     | otherwise = elem' a xs

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
    let smallerOrEqual = [a | a <- xs, a <= x]
        larger = [a | a <-xs, a > x]
    in quickSort smallerOrEqual ++ [x] ++ quickSort larger


multThree :: Int -> Int -> Int -> Int
multThree x y z = x *y * z

multTwoWithNine = multThree 9

-- compareTo100 :: Int -> Ordering
-- compareTo100 x = 100 `compare` x

compareTo100 :: Int -> Ordering
compareTo100 = compare 100

divideBy10 :: (Floating a) => a -> a
divideBy10 = (/10)

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x

flip'' ::  (a -> b -> c) -> (b -> a -> c)
flip'' f y x = f x y

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
   | p x       = x : filter' p xs
   | otherwise = filter' p xs

quickSort2 :: (Ord a) => [a] -> [a]
quickSort2 [] = []
quickSort2 (x:xs) =
    let smallerOrEqual = filter (<= x) xs
        larger         = filter (> x) xs
    in quickSort smallerOrEqual ++ [x] ++ quickSort larger

largestDiv :: Integer
largestDiv = head (filter p [99999,99998..])
    where p x = x `mod` 3829 == 0

collatz :: Integer -> [Integer]
collatz 1 = [1]
collatz n
    | even n = n:collatz (n `div` 2)
    | odd n  = n:collatz (n * 3 +1)

numLongChains :: Int
numLongChains = length (filter isLong (map collatz [1..1000]))
   where isLong xs = length xs > 15