module Chapter8 where

-- 8.2

data Move = North | South | East | West deriving Show

goWest :: Move -> Move
goWest _ = West

