{-# LANGUAGE TemplateHaskell #-}
module Year2021.Solutions.Day06
    ( d6sol1,
      d6sol2
    ) where
import Test.QuickCheck
import Test.QuickCheck.All
import Data.List
import Data.List.Utils
import qualified Data.MultiSet as MS

-- Improved solution using MultiSet
exampleS :: MS.MultiSet Int
exampleS = MS.fromList [3,4,3,1,2]

inputS :: IO (MS.MultiSet Int)
inputS = MS.fromList <$> map read <$> split "," <$> readFile "Year2021/Inputs/Day06.txt"

sol1S :: MS.MultiSet Int -> Int
sol1S = MS.size . runS 80

sol2S :: MS.MultiSet Int -> Int
sol2S = MS.size . runS 256

runS :: Int -> MS.MultiSet Int -> MS.MultiSet Int
runS 0 ms = ms
runS i ms = runS (i-1) $ MS.concatMap checkVal ms

checkVal :: Int -> [Int]
checkVal 0 = [6,8]
checkVal x = [x-1]

-- Original solution -> reinventing MultiSet
example :: [Int]
example = [3,4,3,1,2]

input :: IO [Int]
input = map read <$> split "," <$> readFile "Year2021/Inputs/Day06.txt"

sol1 :: [Int] -> Int
sol1 = calc 80

sol2 :: [Int] -> Int
sol2 = calc 256

calc :: Int -> [Int] -> Int
calc i xs = sum $ snd $ unzip $ run (prepareList xs) [] i

run :: [(Int,Int)] -> [(Int,Int)] -> Int -> [(Int,Int)]
run xs cs 0 = xs ++ cs
run xs cs i = run res reschildren (i-1)
    where
      (nums,left) = part xs
      (mats, children) = part cs
      res = dec left ++ (toTuple 6 (val nums + val mats))
      reschildren = dec children ++ (toTuple 1 $ val nums)

-- Helper functions
prepareList :: [Int] -> [(Int,Int)]
prepareList =  map (\x -> (head x, length x)) . group . sort

toTuple :: Int -> Int -> [(Int,Int)]
toTuple x i | i == 0 = []
            | otherwise = [(x,i)]
part :: [(Int,Int)] -> ([(Int,Int)],[(Int,Int)])
part = partition (\(x,_) -> x == 0)
val :: [(Int,Int)] -> Int
val = sum . map snd
dec :: [(Int,Int)] -> [(Int,Int)]
dec = map (\(x,y) -> (x-1,y))

-- Boilerplate
e1, e2 :: Int
e1 = 5934
e2 = 26984457539

d6sol1 :: IO Int
d6sol1 = sol1 <$> input
d6sol2 :: IO Int
d6sol2 = sol2 <$> input
prop_d6test1 :: Bool
prop_d6test1 = sol1 example == e1
prop_d6test2 :: Bool
prop_d6test2 = sol2 example == e2

-- QuickCheck
return []
check = $quickCheckAll
