{-# LANGUAGE TemplateHaskell #-}
module Year2015.Solutions.Day01
  (
    d1sol1,
    d1sol2
  )
where
import Test.QuickCheck
import Test.QuickCheck.All

input :: IO String
input = readFile "Year2015/Inputs/Day01.txt"

sol1 :: String -> Int
sol1 [] = 0
sol1 (x:xs)
    | x == '(' = 1 + sol1 xs
    | x == ')' = -1 + sol1 xs

sol2 :: String -> Int
sol2 = search 0 0

search :: Int -> Int -> String -> Int
search (-1) z _ = z
search y z (x:xs)
    | x == '(' = search (y + 1) (z+1) xs
    | x == ')' = search (y - 1) (z+1) xs

d1sol1 :: IO Int
d1sol1 = sol1 <$> input
d1sol2 :: IO Int
d1sol2 = sol2 <$> input

prop_test1_1 = sol1 "(())" == 0
prop_test1_2 = sol1 "()()" == 0
prop_test1_3 = sol1 "(()(()(" == 3
prop_test1_4 = sol1 ")())())" == -3
prop_test2_1 = sol2 ")" == 1
prop_test2_2 = sol2 "()())" == 5
-- QuickCheck
return []
check = $quickCheckAll
