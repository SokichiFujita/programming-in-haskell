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
fac'' n | n == 0    = 1
        | n >  0    = n * fac'' (n-1)

-- above fac'' throws exception when arg < 0

-- 6.8.2

sumdown :: Int -> Int
sumdown n | n == 0 = 0
          | n >= 0  = n + sumdown (n-1)


-- 6.8.3

(^^^) :: Int -> Int -> Int
x ^^^ 0 = 1
x ^^^ 1 = x
x ^^^ n = x * x ^^^ (n -1)







