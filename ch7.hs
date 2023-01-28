ex1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
ex1 f p = map f . filter p

all' :: (a -> Bool) -> [a] -> Bool
all' p [] = True
all' p (x:xs) = p x && all' p xs

any' :: (a -> Bool) -> [a] -> Bool
any' p (x:xs) = p x || any' p xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs) | p x = x : takeWhile' p xs
                    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs) | p x = dropWhile' p xs
                    | otherwise = x : xs

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f xs = foldr (\x xs -> (f x):xs) [] xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p xs = foldr (\x xs -> if p x then x : xs else xs) [] xs
                  
dec2int' :: [Int] -> Int
dec2int' xs = foldl (\x y -> x*10 + y) 0 xs