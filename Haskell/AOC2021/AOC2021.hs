module AOC2021 where

import Data.Char (digitToInt)

binStrToInt :: String -> Int
binStrToInt = sum . zipWith (\ e m -> 2^e * m) [0 ..] . map digitToInt . reverse

day1Template :: ([Int] -> [Int]) -> String -> Int
day1Template go = length . filter (<0) . go . map read . words

day1Part1 :: String -> Int
day1Part1 = day1Template $ \ xs -> zipWith (-) xs $ tail xs

day1Part2 :: String -> Int
day1Part2 = day1Template (g . f)
  where
    g xs = zipWith (-) xs $ tail xs
    f xs = zipWith3 (\ x y z -> x + y + z) xs (tail xs) (drop 2 xs)

day2Part1 :: String -> Int
day2Part1 = (\ (x, y) -> x * y) . foldl f (0, 0) . parseDirections
  where
    f t d = d t
    parseDirections = go . words
      where
        go [] = []
        go ("forward":n:xs) = (\ (x, y) -> (x + read n, y)):go xs
        go ("down":n:xs)    = (\ (x, y) -> (x, y + read n)):go xs
        go ("up":n:xs)      = (\ (x, y) -> (x, y - read n)):go xs

day2Part2 :: String -> Int
day2Part2 = (\ (x, y, _) -> x * y) . foldl f (0, 0, 0) . parseDirections
  where
    f t d = d t
    parseDirections = go . words
      where
        go [] = []
        go ("down":n:xs)    = (\ (x, y, a) -> (x, y, a + read n)):go xs
        go ("up":n:xs)      = (\ (x, y, a) -> (x, y, a - read n)):go xs
        go ("forward":n:xs) = (\ (x, y, a) -> (x + read n, y + a * read n, a)):go xs

day3Part1 :: String -> Int
day3Part1 = (\ (g, e) -> g * e) . d3GammaEpsilon

d3GammaEpsilon :: String -> (Int, Int)
d3GammaEpsilon = f3 . foldr f2 ([],[]) . foldr f1 (repeat 0) . words
  where
    f1 cs xs = zipWith (\ c x -> (if c == '1' then succ else pred) x) cs xs
    f2 n (g, e)
      | n > 0     = ('1':g, '0':e)
      | n < 0     = ('0':g, '1':e)
      | otherwise = error "no most-common bit"
    f3 (g, e) = (binStrToInt g, binStrToInt e)

d3test = "00100 11110 10110 10111 10101 01111 00111 11100 10000 11001 00010 01010"

run :: Show a => (String -> a) -> FilePath -> IO ()
run day path = readFile path >>= (putStrLn . show . day)

runD1P1 = run day1Part1 "day1_input.txt"
runD1P2 = run day1Part2 "day1_input.txt"
runD2P1 = run day2Part1 "day2_input.txt"
runD2P2 = run day2Part2 "day2_input.txt"
runD3P1 = run day3Part1 "day3_input.txt"

main :: IO ()
main = putStrLn "hi"
