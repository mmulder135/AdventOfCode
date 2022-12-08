{-# LANGUAGE TemplateHaskell #-}
module Year2022.Solutions.Day0
  (
    d7sol1,
    d7sol2
  )
where
import Test.QuickCheck
import Test.QuickCheck.All
import Data.List (nub)

sol1 :: String -> Int
sol1 = findUniqueWindow 4

sol2 :: String -> Int
sol2 = findUniqueWindow 14

findUniqueWindow :: Int -> String -> Int
findUniqueWindow n str
    | (length $ nub window) < (length window) = 1 + (findUniqueWindow n $ tail str)
    | otherwise = n
    where
      window = take n str


d7sol1 :: IO Int
d7sol1 = sol1 <$> input
d7sol2 :: IO Int
d7sol2 = sol2 <$> input

input :: IO String
input = readFile "Year2022/Inputs/Day06.txt"

prop_sol11 = sol1 "mjqjpqmgbljsphdztnvjfqwrcgsmlb" == 7
prop_sol12 = sol1 "bvwbjplbgvbhsrlpgdmjqwftvncz" == 5
prop_sol13 = sol1 "nppdvjthqldpwncqszvftbrmjlhg" == 6
prop_sol14 = sol1 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" == 10
prop_sol15 = sol1 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" == 11

prop_sol21 = sol2 "mjqjpqmgbljsphdztnvjfqwrcgsmlb" == 19
prop_sol22 = sol2 "bvwbjplbgvbhsrlpgdmjqwftvncz" == 23
prop_sol23 = sol2 "nppdvjthqldpwncqszvftbrmjlhg" == 23
prop_sol24 = sol2 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" == 29
prop_sol25 = sol2 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" == 26

-- QuickCheck
return []
check = $quickCheckAll
