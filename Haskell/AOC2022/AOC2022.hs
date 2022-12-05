module AOC2022 where

import Data.Char (ord, isDigit, isUpper)
import Data.List (sort)
import Data.Maybe (fromJust)

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

day2P1 :: String -> Integer
day2P1 = sum . map f . d2Rounds
  where
    strat = [('X', Rock), ('Y', Paper), ('Z', Scissors)]
    f (opp, c) = d2Score opp $ fromJust $ lookup c strat

day2P2 :: String -> Integer
day2P2 = sum . map f . d2Rounds
  where
    f t@(opp, _) = d2Score opp $ fromJust $ lookup t strat
    strat = [ ((Rock, 'X'), Scissors)
            , (Paper, 'X'), Rock)
            , ((Scissors, 'X'), Paper)
            , ((Rock, 'Y'), Rock)
            , ((Paper, 'Y'), Paper)
            , ((Scissors, 'Y'), Scissors)
            , ((Rock, 'Z'), Paper)
            , ((Paper, 'Z'), Scissors)
            , ((Scissors, 'Z'), Rock)
            ]

data RPS = Rock | Paper | Scissors deriving (Show, Eq)

-- opponent then player
d2Score :: RPS -> RPS -> Integer
d2Score Rock Rock = 3 + 1
d2Score Paper Paper = 3 + 2
d2Score Scissors Scissors = 3 + 3
d2Score Paper Rock = 0 + 1
d2Score Scissors Paper = 0 + 2
d2Score Rock Scissors = 0 + 3
d2Score Scissors Rock = 6 + 1
d2Score Rock Paper = 6 + 2
d2Score Paper Scissors = 6 + 3

d2OpponentMove :: Char -> RPS
d2OpponentMove 'A' = Rock
d2OpponentMove 'B' = Paper
d2OpponentMove 'C' = Scissors
d2OpponentMove _   = error "invalid char"

d2Rounds :: String -> [(RPS, Char)]
d2Rounds = map f . lines
  where
    f [o, _, p] = (d2Opponentmove o, p)

day3P1 :: String -> Integer
day3P1 = sum . map (d3Priority . d3FindDup) . d3Sacks

day3P2 :: String -> Integer
day3P2 = sum . map (d3Priority . d3FindDup3) . d3InThrees . lines

d3FindDup :: (String, String) -> Char
d3FindDup (front, back) = go front
  where
    go (x:xs)
      | elem x back = x
      | otherwise   = go xs
    go ""           = error $ "no duplicate for lists " ++ front ++ " and " ++ back

d3Priority :: Char -> Integer
d3Priority c
  | isUpper c = toInteger $ ord c - 38
  | otherwise = toInteger $ ord c - 96

d3InThrees :: [a] -> (a, a, a)
d3InThrees (x:y:z:xs) = (x, y, z):d3InThrees xs
d3InThrees _          = []

d3FindDup3 :: (String, String, String) -> Char
d3FindDup3 (xs, ys, zs) -> head $ filter (\ x -> elem x ys && elem x zs) xs

day4P1 :: String -> Int
day4P1 = length . filter f . d4Parse
  where
    f (a, b, c, d)
      | a >= c && b <= d = True
      | c >= a && c <= a = True
      | otherwise        = False

day4P2 :: String -> Int
day4P2 = length . filter f . d4Parse
  where
    f (a, b, c, d)
      | b >= c && b <= d = True
      | a <= d && a >= c = True
      | c <= b && c >= a = True
      | d >= a && d <= b = True
      | otherwise        = False

d4Parse :: String -> [(Int, Int, Int, Int)]
d4Parse = map go . lines
  where
    go s =
      let [a, _, b, _, c, _, d] = groupBy (\ x y -> isDigit x == isDigit y) s
      in  (read a, read b, read c, read d)
