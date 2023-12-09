module Year2023.Solutions.Day09 where
import Data.Binary.Get (Decoder(Fail))

example :: String
example = "0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45"
input :: IO String
input = readFile "Year2023/Inputs/Day09.txt"

sol1 :: String -> Int
sol1 = sum . map run . parse

sol2 :: String -> Int
sol2 = sum . map (run . reverse) . parse

run :: [Int] -> Int
run is  | allZeroes is  = 0
        | otherwise     = last is + run (getDiffs is)
    where
        allZeroes :: [Int] -> Bool
        allZeroes [] = True
        allZeroes (0:xs) = allZeroes xs
        allZeroes _ = False

getDiffs :: [Int] -> [Int]
getDiffs = mapAdjacent (flip (-))

mapAdjacent :: (a -> a -> a) -> [a] -> [a]
mapAdjacent f xs = zipWith f xs (tail xs)

parse:: String -> [[Int]]
parse = map (map read . words) . lines