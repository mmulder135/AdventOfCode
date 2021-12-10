{-# LANGUAGE TemplateHaskell #-}
module Year2021.Solutions.Day07
( d7sol1,
  d7sol2
)
where
import Test.QuickCheck
import Test.QuickCheck.All
import Data.List.Utils (split)
import Data.List (sort)

example :: [Int]
example = [16,1,2,0,4,2,7,1,2,14]
input :: IO [Int]
input = map read <$> split "," <$> readFile "Year2021/Inputs/Day07.txt"

sol1 :: [Int] -> Int
sol1 xs = sum $ map cost xs
    where
      cost x = (abs ( x - median xs))

sol2 :: [Int] -> Int
sol2 xs = min (sum $ map roundDown xs) (sum $ map roundUp xs)
    where
      roundDown x = sum [1 .. abs (mean xs - x)]
      roundUp x = sum [1 .. abs (mean xs + 1 - x)]

median :: [Int] -> Int
median xs | odd l     = s !! p
          | otherwise = ((s !! p) + (s !! (p + 1))) `div` 2
    where
      l = length xs
      p = quot (length xs) 2 - 1
      s = sort xs

mean :: [Int] -> Int
mean xs = quot (sum xs) (length xs)

-- Boilerplate
e1, e2 :: Int
e1 = 37
e2 = 168
s1 = 353800
s2 = 98119739
d7sol1 :: IO Int
d7sol1 = sol1 <$> input
d7sol2 :: IO Int
d7sol2 = sol2 <$> input
prop_d7test1 :: Bool
prop_d7test1 = sol1 example == e1
prop_d7test2 :: Bool
prop_d7test2 = sol2 example == e2

-- QuickCheck
return []
check = $quickCheckAll
