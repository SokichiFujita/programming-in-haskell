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














-- 7.8





