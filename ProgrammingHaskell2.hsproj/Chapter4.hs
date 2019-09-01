module Chapter4 where
  
-- 4.1

even' :: Integral a => a -> Bool
even' n = mod n 2 == 0
 
splitAt' :: Int -> [a] -> ([a],[a])
splitAt' n xs = (take n xs, drop n xs)

recip' :: Fractional a => a -> a
recip' n = 1/n

-- 4.2

abs' :: Int -> Int
abs' n = if n >= 0 then n else -n

signum' :: Int -> Int
signum' n = if n < 0 then -1 else
            if n == 0 then 0 else 1

-- 4.3 guard

abs'' :: Int -> Int
abs'' n | n >= 0 = n
        | n < 0  = -n

abs''' :: Int -> Int
abs''' n | n >= 0    =  n 
         | otherwise = -n

signum'' :: Int -> Int
signum'' n | n < 0 = -1
           | n == 0 = 0
           | n >= 0 = 1
           
-- 4.4 pattern match

not :: Bool -> Bool
not False = True
not True = False

(&&&) :: Bool -> Bool -> Bool
True &&& True  = True
_ &&& _ = False

fst' :: (a,b) -> a
fst' (a,_) = a

isStartA :: [Char] -> Bool
isStartA ('A': _) = True
isStartA _ = False

-- 4.5 lambda

add' :: Int -> (Int -> Int)
add' = \x -> (\y -> x + y)


odds :: Int -> [Int]
odds n = map (\x -> x*2 + 1) [0..n-1]


-- 4.8 exercise

-- 4.8.1

halve :: [a] -> ([a], [a])
halve xs = (take ((length xs) `div` 2) xs, drop ((length xs) `div` 2) xs)

-- 4.8.2

-- a
third' :: [a] -> a 
third' xs = head (tail (tail (take 3 xs)))

-- b
third'' :: [a] -> a 
third'' xs = xs !! 2

-- c
third''' :: [a] -> a
third''' (_: _: a: _) = a


-- 4.8.3

-- a
safetail' :: [a] -> [a]
safetail' xs = if null xs then [] else tail xs

-- b
safetail'' :: [a] -> [a]
safetail'' xs | null xs   = [] 
              | otherwise = tail xs

-- c
safetail''' :: [a] -> [a]
safetail''' [] = [] 
safetail''' xs  = tail xs

-- 4.8.4

(|||) :: Bool -> Bool -> Bool
False ||| False  = False
_     ||| _      = True


-- 4.8.5

(&&&&) :: Bool -> Bool -> Bool
x &&&& y = if x == True then (if y == True then True else False) else False














