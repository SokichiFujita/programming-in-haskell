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
