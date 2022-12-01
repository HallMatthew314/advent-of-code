module AOC2022 where

import Data.List (sort)

day1P1 :: String -> Integer
day1P1 = maximum . d1Elves

day1P2 :: String -> Integer
day1P2 = (\ (x, y, z) -> x + y + z) . top3 . d1Elves

top3 :: Ord a => [a] -> (a, a, a)
top3 = (a:b:c:rest) = foldr f (a', b', c') rest
  where
    [c', b', a'] = sort [a, b, c]
    f e t@(x, y, z)
      | e > x     = (e, x, y)
      | e > y     = (x, e, y)
      | e > z     = (x, y, e)
      | otherwise = t

d1Elves :: String -> [Integer]
d1Elves = foldr f [] . lines
  where
    f "" []    = [0]
    f "" es    = 0:es
    f s []     = [read s]
    f s (e:es) = (e + read s):es
