module Chapter7 where
  

-- 7.1

twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- 7.2

map' :: (a -> b) -> [a] -> [b]
map' f l = [f x |x <- l]

filter' :: (a -> Bool) -> [a] -> [a]
filter' f l = [x| x <- l, f x]
