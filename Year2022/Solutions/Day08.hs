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
    len = (length $ head matrix) - 1
    matrix = getIntMatrix input
    transposed = transpose matrix
    visH = Set.filter (isVisibleHorizontal matrix) options
    visV = Set.filter (isVisibleHorizontal transposed . swap) options

isVisibleHorizontal :: [[Int]] -> (Int, Int) -> Bool
isVisibleHorizontal mtx (x,y) = visible left || visible right
    where
      (left, height : right) = splitAt x $ mtx !! y

      visible :: [Int] -> Bool
      visible xs = 0 == (length $ filter (>= height) xs)

sol2 :: [String] -> Int
sol2 input = last $ sort $ scenicScores
  where
    matrix = getIntMatrix input
    options = [ (x,y) | x <- [0..len], y <- [0..height]]
    height = length matrix - 1
    len = (length $ head matrix) - 1
    transposed = transpose matrix
    horizontal = map (getHorizontal matrix) options
    vertical = map (getHorizontal transposed . swap) options
    heights = map (\(x,y) -> matrix !! y !! x) options
    allDirs = zipWith (++) horizontal vertical
    dists = map getDists $ zip heights allDirs
    scenicScores = map (foldl1 (*)) dists
    
getDists :: (Int, [[Int]]) -> [Int]
getDists (height, views) = map (getViewingDist height) views

getHorizontal :: [[Int]] -> (Int, Int) -> [[Int]] -- get horizontal view from coord
getHorizontal mtx (x,y) = (\(l, r) -> reverse l : tail r : []) $ splitAt x $ mtx !! y

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
prop_sol2 = sol2 example == 8
-- QuickCheck
return []
check = $quickCheckAll
