{-# LANGUAGE TemplateHaskell #-}
module Year2022.Solutions.Day01
  (
    d1sol1,
    d1sol2
  )
where
import Test.QuickCheck
import Test.QuickCheck.All
import Data.List


example :: [[Int]]
example = [[1000, 2000, 3000], [4000], [5000, 6000], [7000, 8000, 9000], [10000]]

parse :: String -> [[Int]]
parse x = parseLines (lines x) []
    where
      parseLines :: [String] -> [Int] -> [[Int]]
      parseLines [] tmp       = [tmp]
      parseLines ("":xs) tmp  = tmp : parseLines xs []
      parseLines (x:xs) tmp   = parseLines xs (read x : tmp)

sol1 :: [[Int]] -> Int
sol1 = maximum . map sum

sol2 :: [[Int]] -> Int
sol2 = sum . take 3 . reverse . sort . map sum
input :: IO String
input = readFile "Year2022/Inputs/Day01.txt"

d1sol1 :: IO Int
d1sol1 = sol1 <$> parse <$> input
d1sol2 :: IO Int
d1sol2 = sol2 <$> parse <$>input

prop_d1test1 :: Bool
prop_d1test1 = sol1 example == 24000

prop_d1test2 :: Bool
prop_d1test2 = sol2 example == 45000
-- QuickCheck
return []
check = $quickCheckAll
