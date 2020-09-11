module AOC2015 where

import Data.List.Split (splitOn)

-- Day 1
d1Map :: Char -> Int
d1Map '(' =  1
d1Map ')' = -1
d1Map _   =  0
-- The last case should be `undefined` but
-- I don't want to learn how IO monad works

day1Part1 :: String -> Int
day1Part1 = sum . map d1Map

day1Part2 :: String -> Int
day1Part2 = length . takeWhile (>=0) . scanl (+) 0 . map d1Map

-- Day 2
d2Parse :: String -> [[Int]]
d2Parse s = map (map read . splitOn "x") $ lines s

d2Area :: [Int] -> Int
d2Area ds =
    let as = map (\t -> fst t * snd t) $ zip ds (last ds:init ds)
    in  sum as * 2 + minimum as

d2Ribbon :: [Int] -> Int
d2Ribbon ds = 2 * (sum ds - maximum ds) + product ds

day2Part1 :: String -> Int
day2Part1 = sum . map d2Area . d2Parse

day2Part2 :: String -> Int
day2Part2 = sum . map d2Ribbon . d2Parse

-- Helper functions for running on input files.
run :: Show a => (String -> a) -> FilePath -> IO ()
run day path = do
    inp <- readFile path
    putStrLn $ show $ day inp

runD1P1 = run day1Part1 "day1_input.txt"
runD1P2 = run day1Part2 "day1_input.txt"
runD2P1 = run day2Part1 "day2_input.txt"
runD2P2 = run day2Part2 "day2_input.txt"

