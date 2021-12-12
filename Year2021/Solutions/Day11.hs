{-# LANGUAGE TemplateHaskell #-}
module Year2021.Solutions.Day11
  (
    d11sol1,
    d11sol2
  )
where
import Test.QuickCheck
import Test.QuickCheck.All
import qualified Data.Map as M
import qualified Data.List (concatMap)
import Data.Char (digitToInt)

type Floor = M.Map (Int,Int) Int

sol1 :: [[Int]] -> Int
sol1 = calc 100 . translate

sol2 :: [[Int]] -> Int
sol2 = findFirst 0 . translate

translate :: [[Int]] -> Floor
translate = M.fromList . concatMap (\(x,ls) -> map (\(y,v) -> ((x,y),v)) ls).  zip [0..] .  map (zip [0..])

findFirst :: Int -> Floor -> Int
findFirst c m
    | M.size fil == M.size m = c
    | otherwise = findFirst (c+1) $ snd $ step m
    where
      fil = M.filter (==0) m

calc :: Int -> Floor -> Int
calc 0 _ = 0
calc i m = c + calc (i-1) newm
    where
      (c,newm) = step m

step :: Floor -> (Int,Floor)
step m = (counter,nextmap)
  where
    newmap = flash $ M.map (+1) m
    nextmap = M.map (\x -> if x == -1 then 0 else x) newmap
    counter = M.size $ M.filter (==(-1)) newmap

flash :: Floor -> Floor
flash m | fil == M.empty = m
        | otherwise = flash $ update ns umap
          where
            fil = M.filter (>9) m
            umap = M.map (\x -> if x > 9 then -1 else x) m
            ns = concatMap neighbors $ M.keys fil :: [(Int,Int)]

update :: [(Int,Int)] -> Floor -> Floor
update [] m = m
update (x:xs) m = update xs $ M.adjust (updateVal) x m

updateVal :: Int -> Int
updateVal x
    | x == -1 = -1
    | otherwise = x + 1

neighbors :: (Int,Int) -> [(Int,Int)]
neighbors (x,y) = concatMap (\z -> zip (neighborsV x) $ repeat z) $ neighborsV y

neighborsV :: Int -> [Int]
neighborsV x
    | x == 0 = [0,1]
    | x == 9  = [8,9]
    | otherwise = [x-1,x,x+1]

d11sol1 :: IO Int
d11sol1 = sol1 <$> input
d11sol2 :: IO Int
d11sol2 = sol2 <$> input

input :: IO [[Int]]
input =  map (map digitToInt) <$> lines <$> readFile "Year2021/Inputs/Day11.txt"

example :: [[Int]]
example = [[5,4,8,3,1,4,3,2,2,3],[2,7,4,5,8,5,4,7,1,1],[5,2,6,4,5,5,6,1,7,3],[6,1,4,1,3,3,6,1,4,6],[6,3,5,7,3,8,5,4,7,8],[4,1,6,7,5,2,4,6,4,5],[2,1,7,6,8,4,1,7,2,1],[6,8,8,2,8,8,1,1,3,4],[4,8,4,6,8,4,8,5,5,4],[5,2,8,3,7,5,1,5,2,6]]
example1 =[[6,5,9,4,2,5,4,3,3,4],[3,8,5,6,9,6,5,8,2,2],[6,3,7,5,6,6,7,2,8,4],[7,2,5,2,4,4,7,2,5,7],[7,4,6,8,4,9,6,5,8,9],[5,2,7,8,6,3,5,7,5,6],[3,2,8,7,9,5,2,8,3,2],[7,9,9,3,9,9,2,2,4,5],[5,9,5,7,9,5,9,6,6,5],[6,3,9,4,8,6,2,6,3,7]]
example2 =[[8,8,0,7,4,7,6,5,5,5],[5,0,8,9,0,8,7,0,5,4],[8,5,9,7,8,8,9,6,0,8],[8,4,8,5,7,6,9,6,0,0],[8,7,0,0,9,0,8,8,0,0],[6,6,0,0,0,8,8,9,8,9],[6,8,0,0,0,0,5,9,4,3],[0,0,0,0,0,0,7,4,5,6],[9,0,0,0,0,0,0,8,7,6],[8,7,0,0,0,0,6,8,4,8]]

prop_calc1 = 0 == (calc 1 $ translate example)
prop_step = (translate example1) == (snd $ step $ translate example)
prop_step2 = (translate example2) == (snd $ step $ translate example1)
prop_calc2 = 35 == (calc 2 $ translate example)
prop_calc10 = 204 == (calc 10 $ translate example)
prop_calc100 = 1656 == (calc 100 $ translate example)
prop_sol1 = 1656 == (sol1 example)
prop_sol2 =  195 == (sol2 example)
-- QuickCheck
return []
check = $quickCheckAll
