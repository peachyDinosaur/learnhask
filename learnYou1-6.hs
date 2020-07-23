import Data.List

-- CHAPTER 1

doubleMe x = x+x

--doubleUs x y = x *2 + y*2

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                        then x
                        else x*2

doubleSmallNumber' x = (doubleSmallNumber x) + 1

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

listMulti x y =  [a*b | a <- x, b <- y, a*b > 50]

length' xs = sum[1 | _ <- xs]

-- removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]  

removeOddsNested xxs = [ [x | x <- xs, even x] | xs <- xxs]

rightTriangle = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], 
                    a^2 + b^2 == c^2,
                    a+b+c ==24 ]

-- CHAPTER 2

-- removeNonUppercase :: [Char] -> [Char]
removeNonUppercase :: String -> String
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]


-- CHAPTER 3

factorial :: (Integral a ) => a -> a
factorial 0 = 1
factorial x = x * factorial (x-1)

addVector :: (Num a) => (a,a) -> (a,a) -> (a, a)
addVector a b = (fst a + fst b , snd a + snd b)

addVector' :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVector' (x1, y1) (x2, y2) = (x1+x2, y1+y2)

length'' :: (Num b) => [a] -> b
length'' [] = 0
length'' (_:xs) = 1 + length'' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- bmiTell :: (RealFloat a) => a -> String
-- bmiTell bmi
--     | bmi <= 18.5 = "You underweight"
--     | bmi <= 25.0 = "Normal normal"
--     | bmi <= 30.0 =  "You a big one"
--     | otherwise   = "Uh oh..." 

max' :: (Ord a) => a -> a -> a
max' a b
    |a > b     = a
    |otherwise = b


bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "You underweight"
    | bmi <= normal = "Normal normal"
    | bmi <= fat =  "You a big one"
    | otherwise   = "Uh oh..." 
    where bmi = weight/height ^ 2
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0  

initals :: String -> String -> String
initals firstName lastName =  [f]  ++ "." ++  [l] ++ "."
    where (f:_) = firstName
          (l:_) = lastName

calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

-- CHAPTER 4

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "max of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)  
-- maximum' (x:xs)
--     | x > maxtail = x
--     | otherwise = maxtail
--     where maxtail = maximum' xs

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x: replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip'[] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys 

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = elem' a xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallersorted = quicksort [a | a <-xs, a <=x]
        biggersorted = quicksort [a | a <-xs, a > x]
    in smallersorted ++ [x] ++ biggersorted



-- CHAPTER 5

multiThree :: (Num a) => a -> a -> a -> a  
multiThree x y z = x*y*z

multiplyByNine = multiThree 9

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

mByTwo :: Num a => a -> a -> a
mByTwo x y = x*2 + y*3


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y

flip'' :: (a -> b -> c) -> (b -> a -> c)
flip'' f = g
    where g x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs


quicksort' :: (Ord a) => [a] -> [a]    
quicksort' [] = []    
quicksort' (x:xs) =     
    let smallerSorted = quicksort' (filter (<=x) xs)  
        biggerSorted = quicksort' (filter (>x) xs)   
    in  smallerSorted ++ [x] ++ biggerSorted  

largeDivisable :: (Integral a) => a
largeDivisable = head(filter p [100000, 99999.. ])
    where p x = mod x 3829 == 0

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n = n:chain (n*3 +1)

numLongChain :: Int
numLongChain = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

numLongChain' :: Int
numLongChain' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

sum'' :: (Num a) => [a] -> a
sum'' xs = foldl (\acc x -> acc +x) 0 xs

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' y ys = foldl(\acc x -> if x == y then True else acc) False ys

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs

sqrtSums :: Int  
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- CHAPTER 6

numUnique :: (Eq a) => [a] -> Int
numUnique = length . nub

-- CHAPTER 6 is mainly just showing modules in haskell so no real exercises here

















