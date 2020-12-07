module AOC2015 where

import qualified Data.Set as Set
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString as BS

import Data.ByteArray.Encoding
import Crypto.Hash

import Data.List (isPrefixOf)
import Data.List.Split (splitOn)

-- General utility functions

-- Returns a string with no newline characters.
-- This is needed because readFile includes
-- line feeds at the end of files.
oneLine :: String -> String
oneLine = filter (/= '\n')

-- Only supports lowercase letters
isVowel :: Char -> Bool
isVowel c = elem c "aeiou"

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
        bytes  = UTF8.fromString s           :: BS.ByteString
        digest = hashWith MD5 bytes          :: Digest MD5
        hex    = convertToBase Base16 digest :: BS.ByteString
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

-- Day 5
-- Speed could be improved with a scan instead of a filter
d5ThreeVowels :: String -> Bool
d5ThreeVowels w = length (filter isVowel w) >= 3

d5Consec :: Eq a => [a] -> Bool
d5Consec (x:y:xs) =
    if x == y
        then True
        else d5Consec (y:xs)
d5Consec _ = False

d5NoBadPairs :: String -> Bool
d5NoBadPairs (x:y:xs) =
    if xy == "ab" || xy == "cd" || xy == "pq" || xy == "xy"
        then False
        else d5NoBadPairs (y:xs)
    where xy = [x,y]
d5NoBadPairs _ = True

d5Double :: Eq a => [a] -> Bool
d5Double (x:y:z:xs) =
    if x == z
        then True
        else d5Double (y:z:xs)
d5Double _ = False

d5SubSeq :: Eq a => [a] -> [a] -> Bool
d5SubSeq p xs
    | length p > length xs = False
    | isPrefixOf p xs      = True
    | otherwise            = d5SubSeq p $ tail xs

d5Pair :: Eq a => [a] -> Bool
d5Pair (x:y:xs) =
    if d5SubSeq [x,y] xs
        then True
        else d5Pair (y:xs)
d5Pair _ = False

d5P1Nice :: String -> Bool
d5P1Nice w = d5ThreeVowels w && d5Consec w && d5NoBadPairs w

d5P2Nice :: String -> Bool
d5P2Nice w = d5Double w && d5Pair w

day5Part1 :: String -> Int
day5Part1 = length . filter d5P1Nice . words

day5Part2 :: String -> Int
day5Part2 = length . filter d5P2Nice . words

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
runD5P1 = run day5Part1 "day5_input.txt"
runD5P2 = run day5Part2 "day5_input.txt"

