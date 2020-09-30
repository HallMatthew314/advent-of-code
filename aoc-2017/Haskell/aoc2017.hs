module AOC2017 where

import Data.Char (toLower, digitToInt)

-- General utility functions
-- These might be useful in other problems.

-- Returns a string with no newline characters.
-- This is needed because readFile includes
-- line feeds at the end of files.
oneLine :: String -> String
oneLine = filter (/= '\n')

isVowel :: Char -> Bool
isVowel = (`elem` "aeiou") . toLower

eql :: Eq a => (a, a) -> Bool
eql (x, y) = x == y

halves :: [a] -> ([a], [a])
halves xs = (take h xs, drop h xs)
  where h = length xs `div` 2

absDiv :: Integral a => a -> a -> a
absDiv x y =
  if x < y
    then div y x
    else div x y

areFactors :: Int -> Int -> Bool
areFactors x y =
  if y < x
    then mod x y == 0
    else areFactors y x

-- Day 1
day1Part1 :: String -> Int
day1Part1 s = sum $ map (digitToInt . fst) $ filter eql $ pairs
  where ol    = oneLine s
        pairs = zip ol $ tail $ cycle ol

day1Part2 :: String -> Int
day1Part2 s = (*2) $ sum $ map (digitToInt . fst) $ filter eql $ pairs
  where ol       = oneLine s
        (h1, h2) = halves ol
        pairs    = zip h1 h2

-- Day 2
d2ParseSheet :: String -> [[Int]]
d2ParseSheet = map ((map read) . words) . lines

day2Part1 :: String -> Int
day2Part1 = sum . map (\xs -> maximum xs - minimum xs) . d2ParseSheet

-- Assumes a pair of factors in input list.
d2FindFactors :: [Int] -> [Int]
d2FindFactors [x,y] = [x,y]
d2FindFactors (x:xs) =
  if null facts
    then d2FindFactors xs
    else x:facts
  where
    facts = filter (areFactors x) xs

day2Part2 :: String -> Int
day2Part2 = sum . map f . d2ParseSheet
  where f = (\[x,y] -> absDiv x y) . d2FindFactors

-- Helper functions for running on input files.
run :: Show a => (String -> a) -> FilePath -> IO ()
run day path = do
    inp <- readFile path
    putStrLn $ show $ day inp

runD1P1 = run day1Part1 "day1_input.txt"
runD1P2 = run day1Part2 "day1_input.txt"
runD2P1 = run day2Part1 "day2_input.txt"
runD2P2 = run day2Part2 "day2_input.txt"
--runD3P1 = run day3Part1 "day3_input.txt"
--runD3P2 = run day3Part2 "day3_input.txt"
--runD4P1 = run day4Part1 "day4_input.txt"
--runD4P2 = run day4Part2 "day4_input.txt"
--runD5P1 = run day5Part1 "day5_input.txt"
--runD5P2 = run day5Part2 "day5_input.txt"

