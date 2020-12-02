module AOC2020 where

import Data.Char (isDigit)

intList :: String -> [Int]
intList = map read . words

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _        = False

day1Part1 :: String -> Int
day1Part1 = mul . first ((==2020) . add) . uniquePairs . intList
  where
    add (x,y)  = x + y 
    mul (x,y)  = x * y
    first p xs = head $ filter p xs

uniquePairs :: [a] -> [(a,a)]
uniquePairs xs | length xs < 2 = error "Need at least 2 items"
uniquePairs [x,y]  = [(x,y)]
uniquePairs (x:xs) = [(x,y) | y <- xs] ++ uniquePairs xs

day1Part2 :: String -> Int
day1Part2 = undefined

uniqueTriples :: [a] -> [(a,a,a)]
uniqueTriples xs | length xs < 3 = error "Need at least 3 items"
uniqueTriples [x,y,z] = [(x,y,z)]
uniqueTriples (x:xs) = undefined

data Policy = Policy Int Int Char
  deriving (Show)

meetsPolicy1 :: (Policy, String) -> Bool
meetsPolicy1 ((Policy min' max' c), pass) =
  min' <= count && count <= max'
  where
    count = length $ filter (==c) pass

meetsPolicy2 :: (Policy, String) -> Bool
meetsPolicy2 ((Policy i j c), pass) =
  checkI `xor` checkJ
  where
    checkI = pass !! (i-1) == c
    checkJ = pass !! (j-1) == c

-- Expects format /^\d+-\d+ \w: \w+$/
parsePolicyAndPass :: String -> (Policy, String)
parsePolicyAndPass s = (Policy min' max' c', pass)
  where
    [range, c, pass] = words s
    min' = read $ takeWhile isDigit range
    max' = read $ tail $ dropWhile isDigit range
    c' = head c

day2Base :: ((Policy, String) -> Bool) -> String -> Int
day2Base f = length . filter (f . parsePolicyAndPass) . lines

day2Part1 :: String -> Int
day2Part1 = day2Base meetsPolicy1

day2Part2 :: String -> Int
day2Part2 = day2Base meetsPolicy2

run :: Show a => (String -> a) -> FilePath -> IO ()
run day path = do
  inp <- readFile path
  putStrLn $ show $ day inp

runD1P1 = run day1Part1 "day1_input.txt"
runD1P2 = run day1Part2 "day1_input.txt"
runD2P1 = run day2Part1 "day2_input.txt"
runD2P2 = run day2Part2 "day2_input.txt"

