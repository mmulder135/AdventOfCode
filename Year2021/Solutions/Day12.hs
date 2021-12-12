{-# LANGUAGE TemplateHaskell #-}
module Year2021.Solutions.Day12
  (
    d12sol1,
    d12sol2
  )
where
import Test.QuickCheck
import Test.QuickCheck.All
import Data.List.Utils (split)
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Char as C

sol1 :: [String] -> Int
sol1 xs = length $ search p [] (parse xs) "start"
        where
          p vis x = x `notElem` vis || (C.isUpper $ head x)

sol2 :: [String] -> Int
sol2 xs = length $ search p [] (parse xs) "start"
          where
             p _ "start" = False
             p vis x = not (hasDouble vis) || x `notElem` vis || (C.isUpper $ head x)
             hasDouble = any (\xs -> length xs > 1) . L.group . L.sort . filter (C.isLower . head)

parse :: [String] -> M.Map String [String]
parse = M.fromListWith (++) . concatMap (\[a,b] -> [(a,[b]),(b,[a])]) . map (split "-")


search :: ([String] -> String -> Bool) -> [String] -> M.Map String [String] -> String -> [[String]]
search p visited _ "end" =  [reverse ("end":visited)]
search p visited m cur = concatMap (search p (nVis) m) next
              where
                next = filter (p nVis) $ m M.! cur :: [String]
                nVis = cur:visited :: [String]

example :: [String]
example = ["start-A","start-b","A-c","A-b","b-d","A-end","b-end"]
example1 = ["dc-end","HN-start","start-kj","dc-start","dc-HN","LN-dc","HN-end","kj-sa","kj-HN","kj-dc"]
example2 = ["fs-end","he-DX","fs-he","start-DX","pj-DX","end-zg","zg-sl","zg-pj","pj-he","RW-he","fs-DX","pj-RW","zg-RW","start-pj","he-WI","zg-he","pj-fs","start-RW"]

input :: IO [String]
input = lines <$> readFile "Year2021/Inputs/Day12.txt"

d12sol1 :: IO Int
d12sol1 = sol1 <$> input
d12sol2 :: IO Int
d12sol2 = sol2 <$> input

-- Tests
prop_s1_1 = 10 == sol1 example
prop_s1_2 = 19 == sol1 example1
prop_s1_3 = 226 == sol1 example2
prop_s2_1 = 36 == sol2 example
prop_s2_2 = 103 == sol2 example1
prop_s2_3 = 3509 == sol2 example2

-- QuickCheck
return []
check = $quickCheckAll
