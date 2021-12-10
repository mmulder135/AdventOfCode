{-# LANGUAGE TemplateHaskell #-}
module Year2021.Solutions.Day05
( d5sol1,
  d5sol2
)
where
import Test.QuickCheck
import Test.QuickCheck.All
import qualified Data.Map as Map
import Data.List (sort, group)

type Coord = (Int,Int)
type Line = (Coord,Coord)
type Grid = [[Int]]

example :: [Line]
example = [((0,9),(5,9)),((8,0),(0,8)),((9,4),(3,4)),((2,2),(2,1)),((7,0),(7,4)),((6,4),(2,0)),((0,9),(2,9)),((3,4),(1,4)),((0,0),(8,8)),((5,5),(8,2))]

input :: IO [Line]
input = parseInput <$> words <$> readFile "Year2021/Inputs/Day05.txt"

-- Favorite solution using List functions
sol1List :: [Line] -> Int
sol1List = sol2List . filter (\((x1, y1), (x2, y2)) ->  x1 == x2 || y1 == y2)

sol2List :: [Line] -> Int
sol2List = length . filter (>1) . map length . group . sort . concatMap points

points :: Line -> [Coord]
points ((x1,y1),(x2,y2))
      | x1 == x2 = map (\x -> (x1,x)) [min y1 y2 .. max y1 y2]
      | y1 == y2 = map (\x -> (x,y1)) [min x1 x2 .. max x1 x2]
      | otherwise = zip [x1, (x1 + signum (x2 - x1)) .. x2] [y1, (y1 + signum (y2 - y1)) .. y2]

-- Alternative solution - better then original solution using Map
sol1Map :: [Line] -> Int
sol1Map ls = Map.size $ Map.filter (> 1) $ filledMap
        where
          filledMap = foldl (\cord p -> Map.insertWith (+) p 1 cord) Map.empty visited
          visited = foldr (++) [] (map points ls)
          filtered = filter (\((x1, y1), (x2, y2)) ->  x1 == x2 || y1 == y2) ls

sol2Map :: [Line] -> Int
sol2Map ls = Map.size $ Map.filter (> 1) $ filledMap
        where
          filledMap = foldl (\cord p -> Map.insertWith (+) p 1 cord) Map.empty visited
          -- visited = foldr (++) [] (map points ls)
          visited = concatMap points ls


-- Original solution
sol1 :: [Line] -> Int
sol1 ls = calcOverlap grid
      where
        grid = drawLines ls empGrid
        empGrid = emptyGrid ls

sol2 :: [Line] -> Int
sol2 ls = calcOverlap grid
      where
        grid = drawLines2 ls empGrid
        empGrid = emptyGrid ls

drawLines :: [Line] -> Grid -> Grid
drawLines [] grid = grid
drawLines (((x1,y1),(x2,y2)):xs) grid
        | x1 == x2 = drawLines xs (drawVerLine x1 y1 y2 grid)
        | y1 == y2 = drawLines xs (drawHorLine y1 x1 x2 grid)
        | otherwise = drawLines xs grid

drawLines2 :: [Line] -> Grid -> Grid
drawLines2 [] grid = grid
drawLines2 (xy@((x1,y1),(x2,y2)):xs) grid
        | x1 == x2 = drawLines2 xs (drawVerLine x1 y1 y2 grid)
        | y1 == y2 = drawLines2 xs (drawHorLine y1 x1 x2 grid)
        | otherwise = drawLines2 xs (drawDiagLine xy grid)

calcOverlap :: Grid -> Int
calcOverlap grid = sum (map length filtered)
    where
      filtered = map (filter (\x -> x > 1)) grid

drawHorLine :: Int -> Int -> Int -> Grid -> Grid
drawHorLine 0 x1 x2 (g:gs) = (inc x1 x2 g) : gs
drawHorLine y x1 x2 (g:gs) = g : drawHorLine (y-1) x1 x2 gs

drawVerLine :: Int -> Int -> Int -> Grid -> Grid
drawVerLine x y1 y2 grid = transpose (drawHorLine x y1 y2 (transpose grid))

drawDiagLine :: Line -> Grid -> Grid
drawDiagLine xy@((x1,y1),(x2,y2)) (g:gs)
      | y1 > 0 && y2 > 0 = g : drawDiagLine ((x1,y1-1),(x2,y2-1)) gs
      | y1 == 0 && y2 /= 0 = (inc x1 x1 g) : drawDiagLine (next xy) gs
      | y1 /= 0 && y2 == 0 = (inc x2 x2 g) : drawDiagLine (next xy) gs
      | y1 == 0 && y2 == 0 = (inc x1 x1 g) : gs
      where
        next ((x1,y1),(x2,y2))
                  | y1 == 0 && x2 > x1 = ((x1+1,y1),(x2,y2-1))
                  | y1 == 0 && x2 < x1 = ((x1-1,y1),(x2,y2-1))
                  | y2 == 0 && x2 > x1 = ((x1,y1-1),(x2-1,y2))
                  | y2 == 0 && x2 < x1 = ((x1,y1-1),(x2+1,y2))

transpose :: Grid -> Grid
transpose [] = []
transpose ([]:xs) = []
transpose x = (map head x) : (transpose (map tail x))

inc :: Int -> Int -> [Int] -> [Int]
inc x1 x2 (g:gs)
    | x1 > 0  && x2 > 0   = g : inc (x1-1) (x2-1) gs
    | x1 == 0 && x2 > 0   = (g+1) : inc 0 (x2-1) gs
    | x2 == 0 && x1 > 0   = (g+1) : inc (x1-1) 0 gs
    | x1 == 0 && x2 == 0  = (g+1) : gs

emptyGrid :: [Line] -> Grid
emptyGrid ls = replicate (hx+1) (replicate (hy+1) 0)
      where
        (hx,hy) = findHighest ls (0,0)

findHighest :: [Line] -> Coord -> Coord
findHighest [] xy = xy
findHighest ls@(((x1,y1),(x2,y2)):xs) xy@(hx,hy)
                          | x1 > hx = findHighest ls (x1,hy)
                          | y1 > hy = findHighest ls (hx,y1)
                          | x2 > hx = findHighest ls (x2,hy)
                          | y2 > hy = findHighest ls (hx,y2)
                          | otherwise = findHighest xs xy


-- Parsing
parseInput :: [String] -> [Line]
parseInput [] = []
parseInput (x:y:z:zs) = line : (parseInput zs)
      where
        xs = map (read :: String->Int) (split x)
        ys = map (read :: String->Int) (split z)
        start = (head xs, last xs) :: Coord
        end = (head ys, last ys) :: Coord
        line = (start,end) :: Line

split :: String -> [String]
split x = foldr f [[]]  x
    where
      f c l@(x:xs) | c == ',' = []:l
                   | otherwise = (c:x):xs

d5sol1 :: IO Int
d5sol1 = sol1List <$> input
d5sol2 :: IO Int
d5sol2 = sol2List <$> input
prop_d5test1 :: Bool
prop_d5test1 = sol1List example == 5
prop_d5test2 :: Bool
prop_d5test2 = sol2List example == 12

-- QuickCheck
return []
check = $quickCheckAll
