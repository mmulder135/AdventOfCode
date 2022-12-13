{-# LANGUAGE TemplateHaskell #-}
module Year2022.Solutions.Day0
  (
    d8sol1,
    d8sol2
  )
where
import Test.QuickCheck
import Test.QuickCheck.All
import Data.List                  (transpose, sort)
import Data.Tuple                 (swap)
import qualified Data.Set as Set  (size, fromList, filter, union)


sol1 ::[String] -> Int
sol1 input = Set.size $ Set.union visH visV
  where
    options = Set.fromList [ (x,y) | x <- [0..len], y <- [0..height]]
    height = length matrix - 1
    len = (length $ head matrix) -1
    matrix = getIntMatrix input
    transposed = transpose matrix
    visH = Set.filter (isVisibleH matrix) options
    visV = Set.filter (isVisibleH transposed . swap) options

sol2 :: [String] -> Int
sol2 input = last $ sort $ map (scenicScore matrix) options
  where
    matrix = getIntMatrix input
    options = [ (x,y) | x <- [0..len], y <- [0..height]]
    height = length matrix - 1
    len = (length $ head matrix) -1

isVisibleH :: [[Int]] -> (Int, Int) -> Bool
isVisibleH mtx (x,y) = visible left || visible right
    where
      row = mtx !! y
      (left, r) = splitAt x row
      right = tail r
      height = row !! x
      visible :: [Int] -> Bool
      visible xs = (length $ filter (>= height) xs) == 0

scenicScore :: [[Int]] -> (Int, Int) -> Int 
scenicScore mtx (x,y) = (amountVisibleH mtx (x,y)) * (amountVisibleH (transpose mtx) (y,x))

amountVisibleH :: [[Int]] -> (Int, Int) -> Int
amountVisibleH mtx (x,y) = foldl1 (*) $ map (getViewingDist height) [left, right]
  where
    row = mtx !! y
    (l, r) = splitAt x row
    right = tail r
    left = reverse l
    height = row !! x

getViewingDist :: Int -> [Int] -> Int
getViewingDist _ [] = 0
getViewingDist h (x:xs)
    | x < h     = 1 + getViewingDist h xs
    | otherwise = 1

-- Parsing
getIntMatrix :: [String] -> [[Int]]
getIntMatrix = reverse . map (map (\x -> read [x] :: Int))

d8sol1 :: IO Int
d8sol1 = sol1 <$> input
d8sol2 :: IO Int
d8sol2 = sol2 <$> input

input :: IO [String]
input = lines <$> readFile "Year2022/Inputs/Day08.txt"

example :: [String]
example = [
    "30373",
    "25512",
    "65332",
    "33549",
    "35390"
  ]

prop_parse = getIntMatrix example == [[3,5,3,9,0],[3,3,5,4,9],[6,5,3,3,2],[2,5,5,1,2],[3,0,3,7,3]]
prop_sol1 = sol1 example == 21
prop_vd1 = getViewingDist 5 [3] == 1
prop_vd2 = getViewingDist 5 [5,2] == 1
prop_vd3 = getViewingDist 5 [1,2] == 2
prop_vd4 = getViewingDist 5 [3,5,3] == 2
prop_ss = scenicScore (getIntMatrix example) (2,3) == 4
prop_sol2 = sol2 example == 8
-- QuickCheck
return []
check = $quickCheckAll
