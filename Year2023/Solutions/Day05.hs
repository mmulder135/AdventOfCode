module Year2023.Solutions.Day05 where
import Data.List.Utils (split)

data MapRule = MapRule {destStart :: Int, sourceStart :: Int, rangeLength :: Int} deriving Show
type FullMap = [MapRule]
data Almanac = Almanac {seeds :: [Int], maps :: [FullMap]} deriving Show

example :: IO String
example = readFile "Year2023/Inputs/Day05-example.txt"
input :: IO String
input = readFile "Year2023/Inputs/Day05.txt"

sol1 :: String -> Int
sol1 = minimum . useAlmanac . parse

useAlmanac :: Almanac -> [Int]
useAlmanac (Almanac seeds maps) = map (`runThroughMaps` maps) seeds

runThroughMaps :: Int -> [FullMap] -> Int
runThroughMaps = foldl useMap

useMap :: Int -> FullMap -> Int
useMap i []         = i
useMap i (r:rules)  | diff >= 0 && diff < rangeLength r = diff + destStart r
                    | otherwise                         = useMap i rules
        where
            diff = i - sourceStart r


------------ PART 2 -----------------
type Range = (Int, Int)

-- seeds       : (79,92),           (55,67)
-- soil        : (81,94),           (57,69)
-- fertiliser  : (81,94),           (57,69)
-- water       : (81,94),           (53,56),(61,69)
-- light       : (74,87),           (46,49),    (54,62)
-- temperature : (45,55),(78,80),(82,85),(90,98)
-- humidity    : (46,56),(78,80),(82,85),(90,98)
-- location    : (60,60),(46,55),(82,84),(86,89),(94,96),(56,59),(97,98)

sol2 :: [Char] -> Int
sol2 = minimum . map fst . uncurry (foldl useMapAllSeeds) . parseRange 

useMapAllSeeds :: [Range] -> FullMap -> [Range] -- Use one FullMap on all known ranges
useMapAllSeeds seeds m = concatMap (`useMapRange` m) seeds 

useMapRange :: Range -> FullMap -> [Range]
useMapRange (s, e) []                   = [(s, e)]
useMapRange (s, e) ((MapRule ds ss r):rules)    
        | s >= se || e < ss             = useMapRange (s, e) rules                              -- completely outside range
        | s >= ss && e <  se            = [(s + diff, e + diff)]                                -- completely inside range
        | s >= ss && e >= se            = (s + diff, ds + r - 1) : useMapRange (se, e) rules    -- only first part in range
        | s <  ss && e >= ss && e < se  = (ds, e + diff) : useMapRange (s, ss - 1) rules        -- only last part in range
        | s <  ss && e >= ss && e >= se = (ds, ds + r - 1) : useMapRange (s, ss - 1) rules ++ useMapRange (se, e) rules -- middle part in range
        where
            diff = ds - ss
            se = ss + r 

parseRange :: String -> ([Range], [FullMap])
parseRange input = (makeSeedRanges seeds, maps)
    where
        Almanac seeds maps = parse input

makeSeedRanges :: [Int] -> [Range]
makeSeedRanges [] = []
makeSeedRanges (x:y:xs) = (x, x + y - 1) : makeSeedRanges xs

------ PARSING  ----------
parse :: String -> Almanac
parse input = Almanac seeds maps
    where
        (seedStr:mapStrs) = split "\n\n" input
        seeds = parseSeeds seedStr
        maps = map parseMap mapStrs

parseSeeds :: String -> [Int]
parseSeeds = map read . tail . words 

parseMap :: String -> FullMap
parseMap = map parseMapRule . tail . lines

parseMapRule :: String -> MapRule
parseMapRule input = MapRule d s r
    where
        (d:s:r:_) = map read $ words input