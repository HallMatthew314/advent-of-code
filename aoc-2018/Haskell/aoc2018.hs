module AOC2018 where

import qualified Data.Set as Set

-- General utility functions

-- Returns a string with no newline characters.
-- This is needed because readFile includes
-- line feeds at the end of files.
oneLine :: String -> String
oneLine = filter (/= '\n')

firstDuplicate :: Ord a => [a] -> Maybe a
firstDuplicate xs = dup xs Set.empty
  where
    dup [] _     = Nothing
    dup (x:xs) s =
      if Set.member x s
        then Just x
        else dup xs $ Set.insert x s

-- Day 1
d1Deltas :: String -> [Int]
d1Deltas = map read . words . filter (/='+')

day1Part1 :: String -> Int
day1Part1 = sum . d1Deltas

day1Part2 :: String -> Int
day1Part2 s =
  case d of
    (Just x) -> x
    Nothing  -> undefined
    -- Given that the list is infinite, it should be
    -- impossible to get a `Nothing` here.
  where
    d = firstDuplicate $ scanl (+) 0 $ cycle $ d1Deltas s

-- Helper functions for running on input files.
run :: Show a => (String -> a) -> FilePath -> IO ()
run day path = do
    inp <- readFile path
    putStrLn $ show $ day inp

runD1P1 = run day1Part1 "day1_input.txt"
runD1P2 = run day1Part2 "day1_input.txt"

