module Chapter7 where

import Data.Char
import Data.List

-- 7.1

twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- 7.2

map' :: (a -> b) -> [a] -> [b]
map' f l = [f x |x <- l]

filter' :: (a -> Bool) -> [a] -> [a]
filter' f l = [x| x <- l, f x]

-- 7.3

sum' :: Num a => [a] -> a
sum' = foldr (+) 0

product' :: Num a => [a] -> a
product' = foldr (*) 1

-- 7.4

sum'' :: Num a => [a] -> a
sum'' = foldl (+) 0

-- 7.5

twice' f = f . f

id' :: a -> a
id' = \x -> x

-- 7.6

type Bit = Int
bin2int :: [Bit] -> Int
bin2int bits = sum [w*b | (w,b) <- zip weights bits]
  where weights = iterate (*2) 1

bin2int' :: [Bit] -> Int
bin2int' = foldr (\x y -> x + 2*y) 0

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 []   = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

-- 7.7

-- 7.7.1

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

result :: Ord a => [a] -> [(Int,a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

winner :: Ord a => [a] -> a
winner = snd . last . result


-- 7.7.2

ballots :: [[String]]
ballots = [["Red", "Green"],
           ["Blue"],
           ["Green", "Red", "Blue"], 
           ["Blue", "Green", "Red"], ["Green"]]
rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
  [c] ->c
  (c:cs) -> winner' (elim c bs)


-- 7.9

-- 7.9.1

mf :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mf f p l = map f (filter p l)

-- 7.9.2

-- a
all' :: (a -> Bool) -> [a] -> Bool
all' f l = length (filter f l) == length l

-- b
any' :: (a -> Bool) -> [a] -> Bool
any' f l = length (filter f l) > 0

-- c

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs) | f x == True = x : takeWhile' f xs
                    | otherwise   = []

-- d

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (x:xs) | f x ==  True = dropWhile' f xs
                    | otherwise   = x : dropWhile' f xs


-- 7.9.3

map'' :: (a -> b) -> [a] -> [b]
map'' f [] = []
map'' f (x:xs) = f x : map'' f xs

map''' :: (a -> b) -> [a] -> [b]
map''' f = foldr (\x xs -> f x : xs) []

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f [] = []
filter'' f (x:xs) | f x == True = x : filter'' f xs 
                  | otherwise   = filter'' f xs

filter''' :: (a -> Bool) -> [a] -> [a]
filter''' f = foldr (\x xs -> if f x == True then x : xs else xs) []


-- 7.9.4

dec2int :: [Int] -> Int
dec2int [] = 0
dec2int (x:xs) = x * 10 ^ length xs + dec2int xs

dec2int' :: [Int] -> Int
dec2int' = foldl (\x xs -> x * 10 + xs) 0

-- 7.9.5

curry :: ((a,b) -> c) -> (a -> b -> c)
curry f = \x y -> f (x,y)

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f = \(x, y) -> f x y

-- 7.9.6

unfold p h t x 
  | p x = []
  | otherwise = h x : unfold p h t (t x)

int2bin' :: Int -> [Bit]
int2bin' 0 = []
int2bin' n = n `mod` 2 : int2bin' (n `div` 2)

int2bin'' :: Int -> [Bit]
int2bin'' = unfold (== 0) (`mod` 2) (`div` 2)


chop8' :: [Bit] -> [[Bit]]
chop8' []   = []
chop8' bits = take 8 bits : chop8' (drop 8 bits)

chop8'' :: [Bit] -> [[Bit]]
chop8'' = unfold (== []) (take 8) (drop 8)

map'''' :: (a -> b) -> [a] -> [b]
map'''' f [] = []
map'''' f (x:xs) = f x : map'''' f xs

map''''' :: (a -> b) -> [a] -> [b]
map''''' f = unfold null (f . head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (\x -> False) id f

-- 7.9.7

-- 7.9.8

-- 7.9.9

-- 7.9.10











