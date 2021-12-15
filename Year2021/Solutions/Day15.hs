{-# LANGUAGE TemplateHaskell #-}
module Year2021.Solutions.Day15
  (
    d15sol1,
    d15sol2
  )
where
import Test.QuickCheck
import Test.QuickCheck.All
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Filter (mapMaybe)
import Data.Tuple (swap)
import Data.Char (digitToInt)
import Data.PSQueue (PSQ)
import qualified Data.PSQueue as Q

type Coord = (Int,Int)
type Chitons = Map Coord Int
type ScoreMap = Map Coord Int

sol1 :: [[Int]] -> Int
sol1 xs = search chitons open goal gscores
  where
    open = [(0,0)] :: [Coord]
    chitons = translate xs :: Chitons
    gscores = M.singleton (0,0) 0 :: ScoreMap
    goal = fst $ M.findMax chitons :: Coord

sol2 :: [[Int]] -> Int
sol2 = sol1 . extend
  where
    extend = extendRight . transpose . extendRight . transpose

extendRight :: [[Int]] -> [[Int]]
extendRight [] = []
extendRight (x:xs) = (x ++ x2 ++ x3 ++ x4 ++ x5) : extendRight xs
    where
       m =  map (\x -> (x `mod` 9) + 1)
       x2 = m x
       x3 = m x2
       x4 = m x3
       x5 = m x4

search :: Chitons -> [Coord] -> Coord -> ScoreMap -> Int
search chitons openset goal gscores
    | c == goal = gscores M.! goal
    | otherwise = search chitons newopen goal newg
    where
      c = fst $ head $ sortOn snd $ zip openset $ mapMaybe (getScore gscores) openset :: Coord
      neighbors = getNeighbors goal c :: [Coord]
      newopen = (openset \\ [c]) ++ (map fst $ M.toList (newg M.\\ gscores)) :: [Coord]
      newg = updateScores chitons neighbors c goal gscores :: ScoreMap

updateScores :: Chitons -> [Coord] -> Coord -> Coord -> ScoreMap -> ScoreMap
updateScores _ [] _ _ gscores = gscores
updateScores chitons (x:xs) current goal gscores
  | x == current = updateScores chitons xs current goal gscores
  | comp tent_g oldscore = updateScores chitons xs current goal newg
  | otherwise = updateScores chitons xs current goal gscores
  where
    tent_g = (gscores M.! current) + (chitons M.! x)
    oldscore = getScore gscores x
    comp x (Just y) = x < y
    comp _ (Nothing) = True
    newg = M.insert x tent_g gscores

getNeighbors :: Coord -> Coord -> [Coord]
getNeighbors (mx,my) c = xs ++ ys
  where
    xs = neighborsV mx c
    ys = map swap $ neighborsV my $ swap c

neighborsV :: Int -> Coord -> [Coord]
neighborsV mx (x,y) = map (\x' -> (x',y)) $ xs
  where
    xs
      | x == 0 = [1]
      | x > mx-1  = [x-1]
      | otherwise = [x-1,x+1]

getScore :: ScoreMap -> Coord -> Maybe Int
getScore sm c = M.lookup c sm

-- Using PSQueue for opensset and only looking at higher neigbors to try to speed up algorithm
sol1' :: [[Int]] -> Int
sol1' xs = search' chitons open goal gscores
 where
   open = Q.singleton (0,0) 1 :: PSQ Coord Int
   chitons = translate xs :: Chitons
   gscores = M.singleton (0,0) 0 :: ScoreMap
   goal = fst $ M.findMax chitons :: Coord

sol2' :: [[Int]] -> Int
sol2' = sol1' . extend
 where
   extend = extendRight . transpose . extendRight . transpose

search' :: Chitons -> PSQ Coord Int-> Coord -> ScoreMap -> Int
search' chitons open goal scores
    | current == goal = scores M.! goal
    | otherwise = search' chitons newopen goal newscores
    where
      current = findMin open :: Coord
      neighbors = getNeighbors' goal current :: [Coord]
      toAdd = M.toList (newscores M.\\ scores) :: [(Coord,Int)]
      newopen = foldr (queueInsert) (Q.delete current open) toAdd :: PSQ Coord Int
      newscores = updateScores chitons neighbors current goal scores :: ScoreMap

findMin :: PSQ Coord Int -> Coord
findMin = Q.key . h . Q.findMin
  where
    h (Just x) = x

queueInsert ::  (Coord,Int) -> PSQ Coord Int -> PSQ Coord Int
queueInsert (c,i) q = Q.insert c i q


getNeighbors' :: Coord -> Coord -> [Coord]
getNeighbors' (mx,my) c = xs ++ ys
  where
    xs = neighborsV' mx c
    ys = map swap $ neighborsV' my $ swap c

neighborsV' :: Int -> Coord -> [Coord]
neighborsV' mx (x,y)
      | x == 0 = [(1,y)]
      | x > mx-1  = []
      | otherwise = [(x+1,y)]

translate :: [[Int]] -> Chitons
translate = M.fromList . concatMap (\(x,ls) -> map (\(y,v) -> ((x,y),v)) ls).  zip [0..] .  map (zip [0..])

example :: [[Int]]
example = [[1,1,6,3,7,5,1,7,4,2],[1,3,8,1,3,7,3,6,7,2],[2,1,3,6,5,1,1,3,2,8],[3,6,9,4,9,3,1,5,6,9],[7,4,6,3,4,1,7,1,1,1],[1,3,1,9,1,2,8,1,3,7],[1,3,5,9,9,1,2,4,2,1],[3,1,2,5,4,2,1,6,3,9],[1,2,9,3,1,3,8,5,2,1],[2,3,1,1,9,4,4,5,8,1]]

input :: IO [[Int]]
input = map (map digitToInt) <$> lines <$> readFile "Year2021/Inputs/Day15.txt"

d15sol1 :: IO Int
d15sol1 = sol1 <$> input
d15sol2 :: IO Int
d15sol2 = sol2 <$> input

prop_sol1 = 40 == sol1 example
prop_sol2 = 315 == sol2 example

prop_sol1' = 40 == sol1' example
prop_sol2' = 315 == sol2' example
-- QuickCheck
return []
check = $quickCheckAll
