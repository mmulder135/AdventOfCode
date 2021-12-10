{-# LANGUAGE TemplateHaskell #-}
module Year2015.Solutions.Day05
  (
    -- d0sol1,
    -- d0sol2
  )
where
import Test.QuickCheck
import Test.QuickCheck.All
import Data.List (isInfixOf)

input :: IO [String]
input = lines <$> readFile "Year2015/Inputs/Day05.txt"

sol1 :: [String] -> Int
sol1 xs = length [1 | str <- xs, isNice str]

isNice :: String -> Bool
isNice str = hasThreeVowels str && hasDouble str && (not $ hasNaughty str)

hasThreeVowels :: String -> Bool
hasThreeVowels xs = 3 <= (length $ filter (\x -> x `elem` vowels) xs)

hasDouble :: String -> Bool
hasDouble (x:y:xs)  | x == y = True
                    | otherwise = hasDouble (y:xs)
hasDouble _ = False

hasNaughty :: String -> Bool
hasNaughty str = True `elem` map (flip isInfixOf str) naugthy

naugthy = ["ab","cd","pq","xy"] :: [String]

vowels = "aeiou" :: String

prop_nice1 = isNice "ugknbfddgicrmopn" == True
prop_nice2 = isNice "aaa" == True
prop_nice3 = isNice "jchzalrnumimnmhp" == False
prop_nice4 = isNice "haegwjzuvuyypxyu" == False
prop_nice5 = isNice "dvszwmarrgswjxmb" == False
prop_sol1 = sol1 ["ugknbfddgicrmopn", "aaa", "jchzalrnumimnmhp", "haegwjzuvuyypxyu", "dvszwmarrgswjxmb"] == 2
-- QuickCheck
return []
check = $quickCheckAll
