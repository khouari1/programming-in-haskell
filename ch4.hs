halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
    where n = length xs `div` 2

-- using head and tail
third1 :: [a] -> a
third1 xs = head (tail (tail xs))

-- using !!
third2 :: [a] -> a
third2 xs = xs !! 2

-- using pattern matching
third3 :: [a] -> a
third3 (_ : _ : x : _) = x

-- using conditional expressions
safetail1 :: [a] -> [a]
safetail1 (x : xs) = if (null xs) then [] else xs

-- using guarded equations
safetail2 :: [a] -> [a]
safetail2 (x : xs) | null xs = []
                   | otherwise = xs

-- using pattern matching
safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 xs = tail xs

mult :: Int -> (Int -> (Int -> Int))
mult = \x -> (\y -> (\z -> x * y * z))

luhnDouble :: Int -> Int
luhnDouble n | nx > 9 = nx - 9
             | otherwise = nx
    where nx = n * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = sum [ax, b, cx, d] `mod` 10 == 0
    where ax = luhnDouble a
          cx = luhnDouble c