module Year2023.Solutions.Day09 where

example :: String
example = "0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45"
input :: IO String
input = readFile "Year2023/Inputs/Day09.txt"

sol1 :: String -> Int
sol1 = sum . map run . parse

sol2 :: String -> Int
sol2 = sum . map (run . reverse) . parse

run :: [Int] -> Int
run is  | all (==0) is  = 0
        | otherwise     = last is + run (getDiffs is)

---------- Alternative using iterate and takeWhile and avoiding reverse
sol1' :: String -> Int
sol1' = sum . map (foldr ((+) . last) 0 . getSeq) . parse

sol2' :: String -> Int
sol2' = sum . map (foldr ((-) . head) 0 . getSeq) . parse

getSeq :: [Int] -> [[Int]]
getSeq = takeWhile (not . all (==0)) . iterate getDiffs
----------------------------------

getDiffs :: [Int] -> [Int]
getDiffs xs = zipWith (-) (tail xs) xs

parse:: String -> [[Int]]
parse = map (map read . words) . lines