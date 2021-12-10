{-# LANGUAGE TemplateHaskell #-}
module Year2015.Solutions.Day02
  (
  d2sol1,
  d2sol2
  )
where
import Test.QuickCheck
import Test.QuickCheck.All
import Data.List.Utils (split)
import Data.List

input :: IO [[Int]]
input = map (map (read::String->Int)) <$> map (split "x") <$> lines <$> readFile "Year2015/Inputs/Day02.txt"

sol1 :: [[Int]] -> Int
sol1 = sum . map calcPresent . map (\[l,w,h] -> [l*w,w*h,h*l])

sol2 :: [[Int]] -> Int
sol2 = sum . map calcBow

calcPresent :: [Int] -> Int
calcPresent s = (minimum s) + (sum (map (*2) s))

calcBow :: [Int] -> Int
calcBow x = (sum $ map (*2) $ delete (maximum x) x) + (foldr (*) 1 x)

d2sol1 :: IO Int
d2sol1 = sol1 <$> input
d2sol2 :: IO Int
d2sol2 = sol2 <$> input

t1 :: [Int] -> Int
t1 = calcPresent . (\[l,w,h] -> [l*w,w*h,h*l])
prop_test1_1 = t1 [2,3,4] == 58
prop_test1_2 = t1 [1,1,10] == 43
prop_test1 = sol1 [[2,3,4],[1,1,10]] == 101
prop_test2_1 = calcBow [2,3,4] == 34
prop_test2_2 = calcBow [1,1,10] == 14

-- QuickCheck
return []
check = $quickCheckAll
