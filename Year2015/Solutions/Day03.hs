{-# LANGUAGE TemplateHaskell #-}
module Year2015.Solutions.Day03
    (
    d3sol1,
    d3sol2
    )
where
import Test.QuickCheck
import Test.QuickCheck.All
import Data.Set

input :: IO String
input = readFile "Year2015/Inputs/Day03.txt"

sol1 :: String -> Int
sol1 = size . deliverPresent (0,0)

sol2 :: String -> Int
sol2 xs = size $ union (deliverPresent (0,0) $ santa xs) (deliverPresent (0,0) $ roboSanta xs)

deliverPresent :: (Int,Int) -> String -> Set (Int,Int)
deliverPresent cs [] = insert cs empty
deliverPresent (x,y) (p:ps)
    | p == '>' = insert (x,y) $ deliverPresent (x+1,y) ps
    | p == '<' = insert (x,y) $ deliverPresent (x-1,y) ps
    | p == '^' = insert (x,y) $ deliverPresent (x,y+1) ps
    | p == 'v' = insert (x,y) $ deliverPresent (x,y-1) ps

roboSanta :: String -> String
roboSanta (x:y:xs) = x : roboSanta xs
roboSanta _ = []

santa :: String -> String
santa (x:y:xs) = y : santa xs
santa _ = []

d3sol1 :: IO Int
d3sol1 = sol1 <$> input
d3sol2 :: IO Int
d3sol2 = sol2 <$> input

prop_test1_1 = sol1 ">" == 2
prop_test1_2 = sol1 "^>v<" == 4
prop_test1_3 = sol1 "^v^v^v^v^v" == 2
prop_test2_1 = sol2 "^v" == 3
prop_test2_2 = sol2 "^>v<" == 3
prop_test2_3 = sol2 "^v^v^v^v^v" == 11

-- QuickCheck
return []
check = $quickCheckAll
