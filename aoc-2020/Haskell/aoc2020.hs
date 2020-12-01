module AOC2020 where

intList :: String -> [Int]
intList = map read . words

day1Part1 :: String -> Int
day1Part1 = (\ (x,y) -> x * y) . d1SumPair 2020 . intList

d1SumPair :: Int -> [Int] -> (Int, Int)
d1SumPair _ xs | length xs < 2 =
  error "No possible pairs, check for bad input"
d1SumPair s (x:xs) = ans res
  where
    res = filter ((==2020) . (+x)) xs
    ans []    = d1SumPair s xs
    ans (p:_) = (x,p)

day1Part2 :: String -> Int
day1Part2 = undefined

run :: Show a => (String -> a) -> FilePath -> IO ()
run day path = do
  inp <- readFile path
  putStrLn $ show $ day inp

runD1P1 = run day1Part1 "day1_input.txt"
runD1P2 = run day1Part2 "day1_input.txt"

