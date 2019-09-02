module Chapter5 where

import Data.Char -- for 5.5

-- List comprehensions

-- 5.1

concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]


firsts :: [(a,b)] -> [a]
firsts ps = [x | (x,_) <- ps]

length' :: [a] -> Int
length' xs = sum [1 | _ <- xs]

-- 5.2 guard

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

findKey :: Eq a => a -> [(a,b)] -> [b]
findKey k t = [v | (k',v) <- t, k == k']

-- 5.3 zip function

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)
 
sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']

-- 5.4

lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

-- 5.5

-- 5.5.1

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

-- 5.5.2

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100


freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
  where n = lowers xs

-- 5.5.3

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
         0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
         6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]
table' = freqs "kdvnhoo lv ixq"

crack :: String -> String
crack xs = encode (-factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab =  [chisqr (rotate n table') table | n <- [0..25]]
    table' = freqs xs


-- 5.7


-- 5.7.2

grid :: Int -> Int -> [(Int,Int)]
grid x y = [(x,y)| x <- [0..x], y <- [0..y]]

-- 5.7.3

square :: Int -> [(Int,Int)]
square x = [(a,b) | (a,b) <- grid x x, a /= b]

-- 5.7.4

replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]] 

-- 5.7.5

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z)| x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]




