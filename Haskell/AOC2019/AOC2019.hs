module AOC2019 where

import qualified Data.Map.Strict as M

import Data.List.Split (splitOn)

import Intcode

intList :: String -> [Int]
intList = map read . words

day1Base :: (Int -> Int) -> String -> Int
day1Base f = sum . map f . intList

day1Part1 :: String -> Int
day1Part1 = day1Base fuel
  where
    fuel x = x `div` 3 - 2

day1Part2 :: String -> Int
day1Part2 = day1Base fuel
  where
    fuel x
      | f > 0     = f + fuel f
      | otherwise = 0
      where
        f = x `div` 3 - 2

day2Part1 :: String -> Int
day2Part1 s = cell 
  where
    cell = viewMemCell 0 mFinished
    mFinished = runMachine m
    m = (newMachine "") { memory = p }
    p = M.insert 1 12 $ M.insert 2 2 $ parseCode s

day2Part2 :: String -> Int
day2Part2 = undefined

run :: Show a => (String -> a) -> FilePath -> IO ()
run day path = do
  inp <- readFile path
  putStrLn $ show $ day inp

runD1P1 = run day1Part1 "day1_input.txt"
runD1P2 = run day1Part2 "day1_input.txt"
runD2P1 = run day2Part1 "day2_input.txt"
runD2P2 = run day2Part2 "day2_input.txt"

