module AOC2021 where

day1Template :: ([Int] -> [Int]) -> String -> Int
day1Template go = length . filter (<0) . go . map read . words

day1Part1 :: String -> Int
day1Part1 = day1Template $ \ xs -> zipWith (-) xs $ tail xs

day1Part2 :: String -> Int
day1Part2 = day1Template (g . f)
  where
    g xs = zipWith (-) xs $ tail xs
    f xs = zipWith3 (\ x y z -> x + y + z) xs (tail xs) (drop 2 xs)

run :: Show a => (String -> a) -> FilePath -> IO ()
run day path = readFile path >>= (putStrLn . show . day)

runD1P1 = run day1Part1 "day1_input.txt"
runD1P2 = run day1Part2 "day1_input.txt"

main :: IO ()
main = putStrLn "hi"
