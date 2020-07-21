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
initals firstName lastName =  capitalize [f]  ++ "." ++ capitalize [l] ++ "."
    where (f:_) = firstName
          (l:_) = lastName

calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

