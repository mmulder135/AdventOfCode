{-# LANGUAGE TemplateHaskell #-}
module Year2022.Solutions.Day03
  (
    d3sol1,
    d3sol2
  )
where
import Test.QuickCheck
import Test.QuickCheck.All
import Data.Char

sol1 :: [String] -> Int
sol1 = sum . map letterToNum . map findDouble

sol2 :: [String] -> Int
sol2 = sum . map letterToNum . map findBadge . splitEvery 3

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs 
  where (as,bs) = splitAt n xs

findDouble :: String -> Char
findDouble str = head $ filter (\x -> x `elem` second) first
        where
          (first, second) = splitAt i str
          i = (length str) `div` 2

letterToNum :: Char -> Int
letterToNum x 
        | isLower x = ord x - 96
        | otherwise = ord x - 38

findBadge :: [String] -> Char
findBadge (first:second:third:xs) = head $ filter (\x -> x `elem` common) third
        where
          common = filter (\x -> x `elem` second) first

d3sol1 :: IO Int
d3sol1 = sol1 <$> input
d3sol2 :: IO Int
d3sol2 = sol2 <$> input

input :: IO [String]
input = lines <$> readFile "Year2022/Inputs/Day03.txt"

example :: [String]
example = ["vJrwpWtwJgWrhcsFMMfFFhFp",
          "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
          "PmmdzqPrVvPwwTWBwg",
          "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
          "ttgJtRGJQctTZtZT",
          "CrZsJsPPZsGzwwsLwLmpwMDw"]

prop_d2test1 = sol1 example == 157
prop_findDouble = (map findDouble example) == "pLPvts"
prop_d3test2 = sol2 example == 70
prop_badges = (map findBadge $ splitEvery 3 example) == "rZ"

-- QuickCheck
return []
check = $quickCheckAll
