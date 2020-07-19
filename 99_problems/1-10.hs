import qualified Data.List as L

-- Problem 1
-- myLast [1,2,3,4]
myLast ::[a] -> a
myLast x = last x

myLast' :: [a] -> a
myLast' [x] = x
myLast' (_:xs) = myLast' xs

-- Problem 2

myButLast :: [a] -> a
myButLast = last . init

myButLast' :: [a] -> a
myButLast' [a, b] = a
myButLast' (_:xs) = myButLast' xs 


myButLast'' :: [a] -> a
myButLast'' [] = error "Empty list"
myButLast'' [x] = error "Too few elements"
myButLast'' (x:xs) = case xs of
    [a] -> x
    _ -> myButLast'' xs

-- Problem 3 

elementAt :: [a] -> Int -> a
elementAt = (!!) 

elementAt' :: [a] -> Int -> a
elementAt' [] _ = error "Empty list"
elementAt' (x:_) 0 = x 
elementAt' (x:xs) e = elementAt' xs (e-1)


-- Problem 4 simple
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = myLength xs +1

-- Problem 4 with helper function
myLength' :: [a] -> Int
myLength' xs = myLengthHelper xs 0

myLengthHelper :: [a] -> Int -> Int
myLengthHelper [] c = c
myLengthHelper (x:xs) c = myLengthHelper xs (c+1)

-- Problem 5

myReverse' :: [a] -> [a]
myReverse'  = foldl (flip (:)) []

myReverse'' :: [a] -> [a]
myReverse'' = reverse


-- Problem 6
isPalindrome ::Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs)
    | x == (last xs) = isPalindrome (init xs)
    | otherwise = False

isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' xs = xs == (reverse xs)

isPalindrome'' []  = True
isPalindrome'' [x] = True
isPalindrome'' xs  = (head xs) == (last xs) && (isPalindrome' $ init $ tail xs)

-- Problem 7
data NestedList a = Elem a | List [NestedList a]

myFlatten :: NestedList a -> [a]
myFlatten (Elem x) = [x]
myFlatten (List x) = concatMap myFlatten x

flatten (Elem a)   = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List [])     = []

-- -- Problem 8
compress :: Eq a => [a] -> [a]
compress = map head . L.group

compress' :: (Eq a) => [a] -> [a]
compress' []     = []
compress' (x:xs) = x : (compress' $ dropWhile (== x) xs)
