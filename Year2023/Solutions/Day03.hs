module Year2023.Solutions.Day03 where
import Data.Char (isDigit)
import Data.Tuple (swap)
import Data.List ( intersect, sort, nub )
import Data.Map (Map)
import qualified Data.Map as M

type Coord = (Int, Int)

example :: [String]
example = ["467..114..","...*......","..35..633.","......#...","617*......",".....+.58.","..592.....","......755.","...$.*....",".664.598.."]

input :: IO [String]
input = lines <$> readFile "Year2023/Inputs/Day03.txt"

sol1 :: [String] -> Int
sol1 input = sum $ map (\(is, str) -> getNum str is 0) $ matchCoordsToStrings input coords
    where
        coords = sort $ partLocs $ giveCoords input

sol2 :: [String] -> Int
sol2 input = sum $ map (product . concatMap toNums . matchCoordsToStrings input) validInts
        where
            coords = giveCoords input
            gearCoords = sort $ getGears coords
            gearNs = map (getNeighbors $ fst $ last coords) gearCoords
            ints = map (intersect $ numberLocations coords ) gearNs
            validInts = filter twoNums ints

            toNums :: ([Int], String) -> [Int]
            toNums (is, str) = getNums str is []



matchCoordsToStrings:: [String] -> [Coord] -> [([Int], String)]
matchCoordsToStrings input coords = zip (map (map snd . \x -> filter ((==x) . fst) coords) [0..length input-1]) input

twoNums :: [Coord] -> Bool
twoNums [x] = False
twoNums xs  | length distinctXs > 1 = True
            | otherwise = distinctYs $ map snd xs
    where
        distinctXs = nub $ map fst xs
        distinctYs :: [Int] -> Bool
        distinctYs [] = False
        distinctYs [x] = False
        distinctYs (x:y:xs) | (x+1) >= y = distinctYs (y:xs)
                            | otherwise = True

getNum :: String -> [Int] -> Int -> Int
getNum _ [] t = t
getNum str (i:is) t     | isDigit $ str !! i = getNum str rest $ num + t
                        | otherwise = getNum str is t
                            where
                                (val, usedIs) = searchNum str i
                                num = read val
                                rest = filter (`notElem` usedIs) is
getNums :: String -> [Int] -> [Int] -> [Int]
getNums _ [] t = t
getNums str (i:is) t    | isDigit $ str !! i = getNums str rest $ num : t
                        | otherwise = getNums str is t
                            where
                                (val, usedIs) = searchNum str i
                                num = read val
                                rest = filter (`notElem` usedIs) is
searchNum :: String -> Int -> (String, [Int])
searchNum str i = (leftRes ++ [str !! i] ++ rightRes, leftIs ++ rightIs)
        where
            (leftRes, leftIs) = searchLeft str (i-1) ("", [])
            (rightRes, rightIs) = searchRight str (i+1) ("", [])

searchLeft :: String -> Int -> (String, [Int]) -> (String, [Int])
searchLeft str i (res, is)  | i < 0 = (res, is)
                            | isDigit t = searchLeft str (i-1) (t : res, i : is)
                            | otherwise = (res, is)
                            where
                                t = str !! i
searchRight :: String -> Int -> (String, [Int]) -> (String, [Int])
searchRight str i (res, is) | i == length str = (res, is)
                            | isDigit t = searchRight str (i+1) (res ++ [t], is ++ [i])
                            | otherwise = (res, is)
                            where
                                t = str !! i

partLocs :: [(Coord, Char)] -> [Coord]
partLocs coords = numberLocations coords `intersect` possiblePartNumber coords

numberLocations :: [(Coord, Char)] -> [Coord]
numberLocations = map fst . filter (isDigit . snd)

possiblePartNumber :: [(Coord, Char)] -> [Coord]
possiblePartNumber coords = concatMap (getNeighbors $ fst $ last coords) $ getSpecialCoords coords

giveCoords :: [String] -> [(Coord, Char)]
giveCoords = concatMap (\(x,ls) -> map (\(y,v) -> ((x,y),v)) ls).  zip [0..] .  map (zip [0..])

getSpecialCoords :: [(Coord, Char)] -> [Coord]
getSpecialCoords = map fst . filter ((/='.') . snd) . filter (not. isDigit. snd)

getNeighbors :: Coord -> Coord -> [Coord]
getNeighbors (mx,my) c = xs ++ ys ++ d
  where
    xs = neighborsV mx c
    ys = map swap $ neighborsV my $ swap c
    d = concatMap (neighborsV mx) ys

neighborsV :: Int -> Coord -> [Coord]
neighborsV mx (x, y)
      | x == 0 = [(1, y)]
      | x == mx  = [(x-1, y)]
      | otherwise = [(x-1, y), (x+1, y)]

getGears :: [(Coord, Char)] -> [Coord]
getGears = map fst . filter ((=='*') . snd)

