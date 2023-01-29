import Data.Char

type Bit = Int

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

curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' f = \x y -> f (x, y)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f = \(x, y) -> f x y

unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold (== []) (take 8) (drop 8)

unfoldMap :: (a -> b) -> [a] -> [b]
unfoldMap f = unfold null (f . head) tail

unfoldIterate :: (a -> a) -> a -> [a]
unfoldIterate f = unfold (const False) f id

-- binary string trasmitter

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

channel :: [Bit] -> [Bit]
channel = id

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

parityNumber :: [Bit] -> Bit
parityNumber xs | hasOddOnes xs = 1
                | otherwise = 0
            where hasOddOnes = odd . length . filter (== 1)

withParityNumber :: [Bit] -> [Bit]
withParityNumber xs = xs ++ [parityNumber xs]

parityIsCorrect :: Bit -> [Bit] -> Bool
parityIsCorrect b xs = parityNumber xs == b

chop :: Int -> [Bit] -> [[Bit]]
chop n = unfold (== []) (take n) (drop n)

verifyBin :: [Bit] -> [Bit]
verifyBin xs | parityNumber unparity == parity = unparity
             | otherwise = error "ERROR!"
          where parity = last xs
                unparity = init xs

decode :: [Bit] -> String
decode = map (chr . bin2int . verifyBin) . chop 9

encode :: String -> [Bit]
encode = concat . map (withParityNumber . make8 . int2bin . ord)

transmit :: String -> String
transmit = decode . channel . encode