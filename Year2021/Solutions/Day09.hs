module Year2021.Solutions.Day09
    (
    d9sol1,
    d9sol2,
    d9test1,
    d9test2
    )
where
import Data.List (transpose, intersect, sort, sortBy)
import qualified Data.Set as Set
import Data.Tuple (swap)
import Data.Char (digitToInt)

example :: [[Int]]
example = [[2,1,9,9,9,4,3,2,1,0],[3,9,8,7,8,9,4,9,2,1],[9,8,5,6,7,8,9,8,9,2],[8,7,6,7,8,9,6,7,8,9],[9,8,9,9,9,6,5,6,7,8]]

input :: IO [[Int]]
input = map (map digitToInt) <$> lines <$> readFile "Year2021/Inputs/Day09.txt"

sol1 :: [[Int]] -> Int
sol1 xs = sum $ map (+1) $  map (\(x,y) -> xs !! x !! y) $ lowPoints xs

sol2 :: [[Int]] -> Int
sol2 xs = foldr (*) 1 $ take 3 $ sortBy (flip compare) $ map (findBasinSize xs)  $ lowPoints xs

lowPoints :: [[Int]] -> [(Int,Int)] -- Intersection of horizontal lowpoints and vertical lowpoints
lowPoints xs = intersect (coords xs) (map swap $ coords $ transpose xs)

coords :: [[Int]] -> [(Int,Int)] -- Given a grid get coordinates of lowPoints looking only in horizontal direction
coords = concatMap (\(x,ys) -> zip (repeat x) $ map fst ys) . zip [0..] . map (filter snd) . map (zip [0..]) . map findLow . map (9:)

findLow :: [Int] -> [Bool] -- True if lower than neignbours, False otherwise. Keeps ordering
findLow [x,y] = [x > y]
findLow (x:y:z:xs) = (y < x && y < z): findLow (y:z:xs)

-- Get basin size of basin of lowPoint c
findBasinSize :: [[Int]] -> (Int,Int) -> Int
findBasinSize xs c = Set.size $ findBasin (Set.empty) (Set.fromList [c]) xs

findBasin :: Set.Set (Int,Int) -> Set.Set (Int,Int) -> [[Int]] -> Set.Set (Int,Int)
findBasin oldSet newSet xs
      | dif == Set.empty = oldSet
      | otherwise = findBasin current newFrontier xs
  where
    current = Set.union oldSet newSet
    dif = Set.difference newSet oldSet
    newFrontier = Set.fromList $ concatMap (\c -> horizontalBasin c xs ++ verticalBasin c xs) dif

-- Find all spots next to (left or right) (x,y) that are part of current basin
horizontalBasin :: (Int,Int) -> [[Int]] -> [(Int,Int)]
horizontalBasin (x,y) xs = map (\(y',v) -> (x,y')) res
  where
    (l,r) = splitAt y $ xs !! x
    (rr,_) = break (==9) r
    (lr,_) = break (==9) $ reverse l
    rres = tail $ zip [y..] rr
    lres = zip [y-1,y-2..] lr
    res = sort $ lres ++ rres

-- Find all spots below or above c that are part of current basin
verticalBasin :: (Int,Int) -> [[Int]] -> [(Int,Int)]
verticalBasin c xs = map swap $ horizontalBasin (swap c) $ transpose xs

e1, e2 :: Int
e1 = 15
e2 = 1134

d9sol1 :: IO Int
d9sol1 = sol1 <$> input
d9sol2 :: IO Int
d9sol2 = sol2 <$> input
d9test1 :: Bool
d9test1 = sol1 example == e1
d9test2 :: Bool
d9test2 = sol2 example == e2
