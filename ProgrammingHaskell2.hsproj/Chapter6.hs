module Chapter6 where


-- 6.1

fac :: Int -> Int
fac n | n == 0    = 1
      | otherwise = n * fac (n-1)

fac' :: Int -> Int
fac' 0 = 1
fac' n = n * fac' (n-1)

-- 6.2

-- guard and pattern match

product' :: Num a => [a] -> a
product' l | []     <- l  = 1
           | (x:xs) <- l  = x * product' xs

-- pattern match only

product'' :: Num a => [a] -> a
product'' []     = 1
product'' (x:xs) = x * product'' xs

-- 6.3

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

(+++) :: [a] -> [a] -> [a]
[] +++ ys = ys
(x:xs) +++ ys = (x : (xs +++ ys))

-- 6.4

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)


qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger  = [b | b <- xs, b > x]

-- 6.5

even' :: Int -> Bool
even' 0 = True
even' n = odd (n-1)

odd' :: Int -> Bool
odd' 0 = False
odd' n = even (n-1)


-- 6.8

-- 6.8.1

-- let a = -1, then fac (-1) doesn't stop.

fac'' :: Int -> Int
fac'' n | n == 0 = 1
        | n >  0 = n * fac'' (n-1)

-- above fac'' throws exception when arg < 0

-- 6.8.2

sumdown :: Int -> Int
sumdown n | n == 0 = 0
          | n >= 0 = n + sumdown (n-1)


-- 6.8.3

(^^^) :: Int -> Int -> Int
x ^^^ 0 = 1
x ^^^ 1 = x
x ^^^ n = x * x ^^^ (n -1)


-- 6.8.4

euclid :: Int -> Int -> Int
euclid x y | x == y = x
           | x >  y = euclid (x - y) y  
           | x <  y = euclid (y - x) x


-- 6.8.6

-- 6.8.6.a

and' :: [Bool] -> Bool
and' l | []         <- l = False
       | [True]     <- l = True
       | (True:xs)  <- l = and' xs
       | (False:xs) <- l = False

-- 6.8.6.b

concat' :: [[a]] -> [a]
concat' []     = []
concat' [[x]]  = [x]
concat' (x:xs) = x ++ concat' xs

-- 6.8.6.c

replicate' :: Int -> a -> [a]
replicate' 1 x = [x]
replicate' n x = [x] ++ replicate' (n-1) x

-- 6.8.6.d

(!!!) :: [a] -> Int -> a
(x: xs) !!! 0 = x
(x: xs) !!! n = xs !!! (n-1)

-- 6.8.6.e

elem' :: Eq a => a -> [a] -> Bool
elem' y []     = False
elem' y (x:xs) | y == x = True
               | y /= x = False

-- 6.8.7

merge :: Ord a => [a] -> [a] -> [a]
merge l [] = l
merge [] l = l
merge (x:xs) (y:ys) | x < y = [x] ++ merge xs (y:ys)
                    | x > y = [y] ++ merge (x:xs) ys
 



