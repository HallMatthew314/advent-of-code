module AOC2020 where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Data.Char (isLower, isDigit, isHexDigit)
import Data.List (sort, intercalate)

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

calcBSP :: Char -> Char -> String -> Int
calcBSP zero one = foldl f 0
  where
    f i c
      | c == one  = i * 2 + 1
      | c == zero = i * 2
      | otherwise = error e
      where
        e = "Bad one or zero value, got " ++ [c]

-- Finally fixed it, got the characters the wrong way around.
boardingPassID :: String -> Int
boardingPassID pass = row * 8 + col
  where
    (passRow,passCol) = splitAt 7 pass
    row = calcBSP 'F' 'B' passRow
    col = calcBSP 'L' 'R' passCol

day5Part1 :: String -> Int
day5Part1 = maximum . map boardingPassID . words

minMaxSum :: [Int] -> (Int,Int,Int)
minMaxSum (x:xs) = foldr f (x,x,x) xs
  where
    f i (min',max',sum') = ( if i < min' then i else min'
                           , if i > max' then i else max'
                           , sum' + i
                           )

triangleNumber :: Int -> Int
triangleNumber n = n * (n + 1) `div` 2

day5Part2 :: String -> Int
day5Part2 s = expected - sum'
  where
    ids = map boardingPassID $ words s
    (min',max',sum') = minMaxSum ids
    expected = triangleNumber max' - triangleNumber (min' - 1)

day6Part1 :: String -> Int
day6Part1 = sum . map f . splitEmptyLines
  where
    f = length . S.fromList . filter (/=' ')

day6Part2 :: String -> Int
day6Part2 = sum . map f . splitEmptyLines
  where
    f = length . foldl1 S.intersection . map S.fromList . words

data Direction = East | North | West | South
  deriving (Enum,Show)

day12Part1 :: String -> Int
day12Part1 = manhattan . fst . foldl f ((0,0), East) . words
  where
    f (p,d) ('E':n) = (move East (read n) p,d)
    f (p,d) ('N':n) = (move North (read n) p,d)
    f (p,d) ('W':n) = (move South (read n) p,d)
    f (p,d) ('S':n) = (move West (read n) p,d)
    f (p,d) ('F':n) = (move d (read n) p,d)
    f (p,d) (c:n) = (p,turn c (read n) d)

manhattan :: Num a => (a,a) -> a
manhattan (x,y) = abs x + abs y

move :: Direction -> Int -> (Int,Int) -> (Int,Int)
move North n (x,y) = (x,y+n)
move South n (x,y) = (x,y-n)
move East n (x,y) = (x+n,y)
move West n (x,y) = (x-n,y)

turn :: Char -> Int -> Direction -> Direction
turn 'L' n d = toEnum $ (div n 90 + fromEnum d) `mod` 4
turn 'R' n d = turn 'L' (-n) d

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
runD6P1 = run day6Part1 "day6_input.txt"
runD6P2 = run day6Part2 "day6_input.txt"
runD12P1 = run day12Part1 "day12_input.txt"

