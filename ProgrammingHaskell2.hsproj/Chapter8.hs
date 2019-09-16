module Chapter8 where

-- 8.2

-- Move

data Move = North | South | East | West deriving (Eq, Ord, Show, Read)

goWest :: Move -> Move
goWest _ = West

-- Shape

data Shape = Circle Float | Rect Float Float deriving (Eq, Ord, Show, Read)

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)

-- 8.3

newtype Nat' = N Int
n1 = N 1


-- 8.4

-- Nat

data Nat = Zero | Succ Nat deriving (Eq, Ord, Show, Read)

nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)


-- List

data List a = Nil | Cons a (List a) deriving (Eq, Ord, Show, Read)

l1 :: List Int
l1 = Cons 3 (Cons 2 (Cons 1 (Nil)))

len :: List a -> Int
len Nil = 0
len (Cons x xs) = 1 + len xs

-- Tree

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Ord, Show, Read)

t1 :: Tree Int
t1 = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node t1 y t2) = x == y || occurs x t1 || occurs x t2

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y)                  = x == y
occurs' x (Node l y r) | x == y     = True
                       | x < y      = occurs' x l
                       | otherwise  = occurs' x r


-- 8.5

data BoolX = F | T deriving (Eq, Ord, Show, Read)

-- 8.6

data Prop = Const Bool 
  | Var Char
  | Not Prop
  | And Prop Prop 
  | Imply Prop Prop deriving (Eq, Ord, Show, Read)


p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

type Assoc k v = [(k,v)]



-- 8.9

-- 8.9.1

mult :: Nat -> Nat -> Nat
mult Zero n = Zero
mult m Zero = Zero
mult (Succ m) n = add (mult m n) n

-- 8.9.2

-- bad implementation: using compare but not effective
occurs'' :: Ord a => a -> Tree a -> Bool
occurs'' x (Leaf y)                         = x == y
occurs'' x (Node l y r) | compare x y == EQ = True
                        | compare x y == LT = occurs'' x l
                        | otherwise         = occurs'' x r

occurs''' :: Ord a => a -> Tree a -> Bool
occurs''' x (Leaf y)     = x == y
occurs''' x (Node l y r) = case compare x y of
                          EQ -> True
                          LT -> occurs''' x l
                          GT -> occurs''' x r

-- occurs''': O(1)
-- occurs, occurs', occurs'' = O(2)


-- 8.9.3





