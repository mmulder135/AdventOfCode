{-# LANGUAGE TemplateHaskell #-}
module Year2021.Solutions.Day13
  (
    d13sol1,
    d13sol2
  )
where
import Test.QuickCheck
import Test.QuickCheck.All
import Data.List.Utils (split)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (groupBy)
import Data.Tuple (swap)

type Input = (Grid,[Instr])
type Grid = Set (Int,Int)
type Instr = (Char,Int)

sol1 :: (Grid,[Instr]) -> Int
sol1 (s,instr) = Set.size $ fold s $ head instr

solve2 :: (Grid,[Instr]) -> Set (Int,Int)
solve2 (s,instr) = foldl fold s instr

sol2 :: IO ()
sol2 = do
  (s,instr) <- input
  let res = foldl fold s instr
  putStrLn $ printPaper res

printPaper :: Grid -> String
printPaper = unlines . map printRow . map (map snd) . groupBy (\a b -> fst a == fst b) . Set.toList . Set.map swap
printRow :: [Int] -> String
printRow xs = [ if x' `elem` xs then '#' else ' ' | x' <- [0 .. maximum xs]]

fold :: Grid -> Instr -> Grid
fold s ('y',y) = Set.union base newLayer
  where
    (base,layer) = Set.partition (\(x,y') -> y' < y) s
    newLayer = Set.map (\(x,y') -> (x,y - (y' - y))) layer
fold s ('x',x) = Set.union base newLayer
  where
    (base,layer) = Set.partition (\(x',y) -> x' < x) s
    newLayer = Set.map (\(x',y) -> (x - (x' - x),y)) layer

example :: (Grid,[Instr])
example = (Set.fromList [(0,3),(0,13),(0,14),(1,10),(2,14),(3,0),(3,4),(4,1),(4,11),(6,0),(6,10),(6,12),(8,4),(8,10),(9,0),(9,10),(10,4),(10,12)],[('y',7),('x',5)])

input :: IO (Grid,[Instr])
input = parse <$> lines <$> readFile "Year2021/Inputs/Day13.txt"

parse :: [String] -> (Grid,[Instr])
parse xs = (coords,instructions)
  where
    [cs,instr] = split [""] xs
    coords = Set.fromList $ map (\[x,y] -> (read x ::Int, read y ::Int)) $ map (split ",") cs
    instructions = map (\[x,y] -> (last x, read y ::Int)) $ map (split "=") instr

d13sol1 :: IO Int
d13sol1 = sol1 <$> input
d13sol2 :: IO ()
d13sol2 = sol2

prop_fold1 = fold (fst example) (head $ snd example) == Set.fromList [(0,0),(0,1),(0,3),(1,4),(2,0),(3,0),(3,4),(4,1),(4,3),(6,0),(6,2),(6,4),(8,4),(9,0),(9,4),(10,2),(10,4)]
prop_sol1 = sol1 example == 17
prop_sol2 = foldl fold (fst example) (snd example) == Set.fromList [(0,0),(0,1),(0,2),(0,3),(0,4),(1,0),(1,4),(2,0),(2,4),(3,0),(3,4),(4,0),(4,1),(4,2),(4,3),(4,4)]
-- QuickCheck
return []
check = $quickCheckAll
