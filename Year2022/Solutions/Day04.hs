{-# LANGUAGE TemplateHaskell #-}
module Year2022.Solutions.Day04
  (
    d4sol1,
    d4sol2
  )
where
import Test.QuickCheck
import Test.QuickCheck.All
import Data.List (intersect)
import Data.List.Utils (split)

sol1 :: [String] -> Int
sol1 = length . filter (\x -> x) . map doesInclude

sol2 :: [String] -> Int
sol2 = length . filter (not . null) . map overlap

doesInclude :: String -> Bool
doesInclude str = (x1 <= y1 && x2 >= y2) || (x1 >= y1 && x2 <= y2)
  where
    [[x1, x2], [y1, y2]] =  parseLine str

-- overlap could also be determined with (x,y),(x1, y1) ===> x >= x1 && x <= y1 || x1 >= x && x1 <= y
overlap :: String -> [Int]
overlap str = intersect fst snd
  where
    [fst, snd] = map (\[x,y] -> drop (x - 1) $ take y $ [1..]) $ parseLine str

parseLine :: String -> [[Int]]
parseLine = map (map (read::String -> Int)) . map (split "-") . split ","


d4sol1 :: IO Int
d4sol1 = sol1 <$> input
d4sol2 :: IO Int
d4sol2 = sol2 <$> input

input :: IO [String]
input = lines <$> readFile "Year2022/Inputs/Day04.txt"

example :: [String]
example = [
      "2-4,6-8",
      "2-3,4-5",
      "5-7,7-9",
      "2-8,3-7",
      "6-6,4-6",
      "2-6,4-8"
    ]


prop_sol1 = sol1 example == 2
prop_sol2 = sol2 example == 4

-- QuickCheck
return []
check = $quickCheckAll
