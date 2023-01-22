import Data.Char

ex1 :: Int
ex1 = sum [ x * x | x <- [1..100] ]

grid :: Int -> Int -> [(Int, Int)]
grid m n = [ (x, y) | x <- [0..m], y <- [0.. n]]

square :: Int -> [(Int, Int)]
square n = [ (x, y) | (x, y) <- grid n n, x /= y ]

replicate' :: Int -> a -> [a]
replicate' n x = [ x | m <- [1..n]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [ (x, y, z) | x <- [1..10], y <- [1..10], z <- [1..10], x^2 + y^2 == z^2 ]

factors :: Int -> [Int]
factors n = [ x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [ x | x <- [1..n], sum (init (factors x)) == x]

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [ x * y | (x, y) <- zip xs ys ]

-- Caesar cipher

let2intLower :: Char -> Int
let2intLower c = ord c - ord 'a'

int2letLower :: Int -> Char
int2letLower n  = chr (ord 'a' + n)

let2intUpper :: Char -> Int
let2intUpper c = ord c - ord 'A'

int2letUpper :: Int -> Char
int2letUpper n  = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2letLower ((let2intLower c + n) `mod` 26)
          | isUpper c = int2letUpper ((let2intUpper c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]