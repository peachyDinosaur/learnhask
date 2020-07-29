-- PROBLEM 21
insertAt :: a -> [a] -> Int -> [a]
insertAt  s xs 1 = s:xs 
insertAt  s (x:xs) n = x : insertAt s xs (n-1)  

-- PROBLEM 22
range x y = [x..y]

-- PROBLEM 26

combinations :: Int -> [a] -> [[a]]
combinations k (x:xs) = (x, take k-1 xs): 