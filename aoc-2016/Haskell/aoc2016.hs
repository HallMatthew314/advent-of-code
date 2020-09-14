module AOC2015 where

-- General utility functions

-- Returns a string with no newline characters.
-- This is needed because readFile includes
-- line feeds at the end of files.
oneLine :: String -> String
oneLine = filter (/= '\n')

-- Day 1
day1Part1 = undefined

-- Helper functions for running on input files.
run :: Show a => (String -> a) -> FilePath -> IO ()
run day path = do
    inp <- readFile path
    putStrLn $ show $ day inp

runD1P1 = run day1Part1 "day1_input.txt"

