data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add m n = int2nat (nat2int m + nat2int n)

mult :: Nat -> Nat -> Nat
mult n Zero = Zero
mult n m = add n (mult n (int2nat (mInt - 1)))
         where mInt = nat2int m - 1

mult' :: Nat -> Nat -> Nat
mult' _ Zero = Zero
mult' m (Succ n) = add m (mult m n)

-- data Tree a = Leaf a | Node (Tree a) a (Tree a)

-- occurs :: Ord a => a -> Tree a -> Bool
-- occurs x (Leaf y) = x == y
-- occurs x (Node l y r) = case compare x y of
--                           EQ -> True
--                           LT -> occurs x l
--                           GT -> occurs x r

data Tree a = Leaf a | Node (Tree a) (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Node (Leaf 4) (Node (Leaf 4) (Leaf 5))))

leavesCount :: Tree a -> Int
leavesCount (Leaf x) = 1
leavesCount (Node x y) = leavesCount x + leavesCount y

balanced :: Tree a -> Bool
balanced (Leaf x) = True
balanced (Node x y) | (abs (leavesCount x - leavesCount y)) < 2 = balanced x && balanced y
                    | otherwise = False