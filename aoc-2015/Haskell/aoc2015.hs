module AOC2015 where

import qualified Data.Set as Set
import qualified Data.ByteString.UTF8 as UTF8

import Data.ByteArray.Encoding
import Crypto.Hash

import Data.List.Split (splitOn)
import Data.ByteString (ByteString)

-- General utility functions

-- Returns a string with no newline characters.
-- This is needed because readFile includes
-- line feeds at the end of files.
oneLine :: String -> String
oneLine = filter (/= '\n')

-- Day 1
d1Map :: Char -> Int
d1Map '(' =  1
d1Map ')' = -1
d1Map _   =  undefined

day1Part1 :: String -> Int
day1Part1 = sum . map d1Map . oneLine

day1Part2 :: String -> Int
day1Part2 = length . takeWhile (>=0) . scanl (+) 0 . map d1Map . oneLine

-- Day 2
d2Parse :: String -> [[Int]]
d2Parse = map (map read . splitOn "x") . lines

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

-- Day 3
d3Map :: Char -> (Int,Int)
d3Map '^' = ( 0, 1)
d3Map 'v' = ( 0,-1)
d3Map '>' = ( 1, 0)
d3Map '<' = (-1, 0)
d3Map _   = undefined

d3AddPair :: Num a => (a,a) -> (a,a) -> (a,a)
d3AddPair p q = (fst p + fst q, snd p + snd q)

d3Points :: String -> Set.Set (Int,Int)
d3Points = Set.fromList . scanl d3AddPair (0,0) . map d3Map

d3TakeEvenIndexes :: [a] -> [a]
d3TakeEvenIndexes []     = []
d3TakeEvenIndexes [x]    = [x]
d3TakeEvenIndexes (x:xs) = x:(d3TakeEvenIndexes $ tail xs)

day3Part1 :: String -> Int
day3Part1 = length . d3Points

day3Part2 :: String -> Int
day3Part2 s =
    let santa = d3Points $ d3TakeEvenIndexes $ oneLine s
        robot = d3Points $ d3TakeEvenIndexes $ tail $ oneLine s
    in  length $ Set.union santa robot

-- Day 4

-- This took way too long to figure out
md5 :: String -> String
md5 s = result
    where
        bytes  = UTF8.fromString s           :: ByteString
        digest = hashWith MD5 bytes          :: Digest MD5
        hex    = convertToBase Base16 digest :: ByteString
        result = UTF8.toString hex           :: String

d4Hash :: (String, Int) -> String
d4Hash (key, i) = md5 (key ++ show i)

d4GoodP1 :: String -> Bool
d4GoodP1 ('0':'0':'0':'0':'0':_) = True
d4GoodP1 _                       = False

d4GoodP2 :: String -> Bool
d4GoodP2 ('0':'0':'0':'0':'0':'0':_) = True
d4GoodP2 _                           = False

day4Part1 :: String -> Int
day4Part1 key = snd $ head $ filter (d4GoodP1 . d4Hash) targets
    where 
        k       = oneLine key
        targets = [(k, i) | i <- [1 .. ]]

day4Part2 :: String -> Int
day4Part2 key = snd $ head $ filter (d4GoodP2 . d4Hash) targets
    where
        k       = oneLine key
        targets = [(k, i) | i <- [1 .. ]]

-- Helper functions for running on input files.
run :: Show a => (String -> a) -> FilePath -> IO ()
run day path = do
    inp <- readFile path
    putStrLn $ show $ day inp

runD1P1 = run day1Part1 "day1_input.txt"
runD1P2 = run day1Part2 "day1_input.txt"
runD2P1 = run day2Part1 "day2_input.txt"
runD2P2 = run day2Part2 "day2_input.txt"
runD3P1 = run day3Part1 "day3_input.txt"
runD3P2 = run day3Part2 "day3_input.txt"
runD4P1 = run day4Part1 "day4_input.txt"
runD4P2 = run day4Part2 "day4_input.txt"

