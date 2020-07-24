-- Problem 14

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = [x,x]++ dupli xs

-- Problem 15

repli  :: [a] -> Int -> [a]
repli  [] _ = []
repli  (x:xs) n = replicate n x ++ repli xs n

-- Problem 16

dropEvery :: [a] -> Int -> [a]
dropEvery xs n
  | length xs < n = xs
  | otherwise     = take (n-1) xs ++ dropEvery (drop n xs) n

-- Problem 17

split xs n = (take n xs, drop n xs)


-- Problem 18
slice xs x y = drop x (take y xs)

-- Problem 19
rotate xs n = take len . drop (n `mod` len) . cycle $ xs
    where len = length xs

-- Problem 20
removeAt 1 (x:xs) = (x, xs)
removeAt n (x:xs) = (l, x:r)
	where (l, r) = removeAt (n - 1) xs