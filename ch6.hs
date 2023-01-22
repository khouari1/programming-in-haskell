fac :: Int -> Int
fac 0 = 1
fac n | n < 0 = 1
      | otherwise = n * fac (n - 1)

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

exp' :: Int -> Int -> Int
exp' n 1 = n
exp' n m = n * (exp' n (m - 1))

euclid :: Int -> Int -> Int
euclid n m | n == m = n
           | n > m = euclid (n - m) m
           | otherwise = euclid (m - n) n

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n - 1) x

bang :: [a] -> Int -> a
bang x 0 = head x
bang x n = bang (drop 1 x) (n - 1)

elem' :: Eq a => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs) | a == x = True
               | otherwise = elem' a xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge ys [] = ys
merge (x:xs) (y:ys) | x < y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

halve :: [a] -> ([a],[a])
halve xs = (take (length xs `div` 2) xs, drop (length xs `div` 2) xs)

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort x1) (msort x2)
    where (x1, x2) = halve xs

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs

last' :: [a] -> a
last' [x] = x
last' (_:xs) = last' xs