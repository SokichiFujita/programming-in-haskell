module Chapter7 where
  

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

