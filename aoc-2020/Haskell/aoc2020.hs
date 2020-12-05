module AOC2020 where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Data.Char (isDigit, isHexDigit)
import Data.List (intercalate)

intList :: String -> [Int]
intList = map read . words

-- This should be a Maybe a but I don't feel like refactoring right now.
first :: (a -> Bool) -> [a] -> a
first p = head . filter p

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _        = False

step :: Int -> [a] -> [a]
step _ []     = []
step n (x:xs) = x:step n (drop (n - 1) xs)

--splitEmptyLines :: String -> [String]
splitEmptyLines = map (intercalate " ") . foldr f [[]] . lines
  where
    f "" (a:acc) = []:a:acc
    f x (a:acc)  = (x:a):acc

day1Part1 :: String -> Int
day1Part1 = mul . first ((==2020) . add) . uniquePairs . intList
  where
    add (x,y)  = x + y 
    mul (x,y)  = x * y

uniquePairs :: [a] -> [(a,a)]
uniquePairs xs | length xs < 2 = error "Need at least 2 items"
uniquePairs [x,y]  = [(x,y)]
uniquePairs (x:xs) = [(x,y) | y <- xs] ++ uniquePairs xs

day1Part2 :: String -> Int
day1Part2 = mul . first ((==2020) . add) . uniqueTriples . intList
  where
    add (x,y,z) = x + y + z
    mul (x,y,z) = x * y * z

uniqueTriples :: [a] -> [(a,a,a)]
uniqueTriples xs | length xs < 3 = error "Need at least 3 items"
uniqueTriples [x,y,z] = [(x,y,z)]
uniqueTriples (x:xs) = ts ++ uniqueTriples xs
  where
    ts = map (\ (a,b) -> (x,a,b)) $ uniquePairs xs

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

day3Part1 :: String -> Int
day3Part1 = countTrees 3 1 . parseTreeMatrix

parseTreeMatrix :: String -> [String]
parseTreeMatrix = map cycle . lines

-- TODO: use list of arrays here
countTrees :: Int -> Int -> [String] -> Int
countTrees x y trees = length $ filter (=='#') $ map f $ zip [0 .. ] trees'
  where
    trees' = step y trees
    f (i, xs) = xs !! (i * x)

day3Part2 :: String -> Int
day3Part2 s = r1d1 * r3d1 * r5d1 * r7d1 * r1d2
  where
    trees = parseTreeMatrix s
    r1d1 = countTrees 1 1 trees
    r3d1 = countTrees 3 1 trees
    r5d1 = countTrees 5 1 trees
    r7d1 = countTrees 7 1 trees
    r1d2 = countTrees 1 2 trees

type Passport = M.Map String String

fieldsPresent :: Passport -> Bool
fieldsPresent = S.isSubsetOf required . S.fromList . M.keys
  where
    required = S.fromList ["hcl","iyr","eyr","ecl","pid","byr","hgt"]

fieldsCorrect :: Passport -> Bool
fieldsCorrect = all fieldCorrect . M.assocs

fieldCorrect :: (String,String) -> Bool
fieldCorrect ("byr",v) = 1920 <= y && y <= 2002
  where
    y = read v :: Int

fieldCorrect ("iyr",v) = 2010 <= y && y <= 2020
  where
    y = read v :: Int

fieldCorrect ("eyr",v) = 2020 <= y && y <= 2030
  where
    y = read v :: Int

fieldCorrect ("hgt",v) = (u == "cm" || u == "in") && inBounds
  where
    h = read $ takeWhile isDigit v :: Int
    u = dropWhile isDigit v
    inBounds =
      if u == "cm"
        then 150 <= h && h <= 193
        else 59 <= h && h <= 76

fieldCorrect ("hcl",'#':v) = length v == 6 && all isHexDigit v

fieldCorrect ("ecl",v) = S.member v colors
  where
    colors = S.fromList ["amb","blu","brn","gry","grn","hzl","oth"]

fieldCorrect ("pid",v) = length v == 9 && all isDigit v

fieldCorrect ("cid",v) = True

fieldCorrect _ = False -- Non-recognised fields will fail

-- "([a-z]+:[a-z]+) *"
passportFromOneLine :: String -> Passport
passportFromOneLine = M.fromList . map f . words
  where
    f s = (k,v)
      where
        k     = takeWhile (/=':') s
        (_:v) = dropWhile (/=':') s

parsePassports :: String -> [Passport]
parsePassports = map passportFromOneLine . splitEmptyLines

day4Part1 :: String -> Int
day4Part1 = length . filter fieldsPresent . parsePassports

day4Part2 :: String -> Int
day4Part2 = length . filter p . parsePassports
  where
    p :: Passport -> Bool
    p pass = fieldsPresent pass && fieldsCorrect pass

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

