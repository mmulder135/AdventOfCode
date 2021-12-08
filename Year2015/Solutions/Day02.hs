module Year2015.Solutions.Day02
  (
  d2sol1,
  d2sol2,
  d2test1,
  d2test2
  )
where
import Data.List.Utils (split)
import Data.List

example :: [[Int]]
example = [[2,3,4],[1,1,10]]
input :: IO [[Int]]
input = map (map (read::String->Int)) <$> map (split "x") <$> lines <$> readFile "Year2015/Inputs/Day02.txt"

sol1 :: [[Int]] -> Int
sol1 = sum . map calcPresent . map (\[l,w,h] -> [l*w,w*h,h*l])

sol2 :: [[Int]] -> Int
sol2 = sum . map calcBow

calcPresent :: [Int] -> Int
calcPresent s = (minimum s) + (sum (map (*2) s))

calcBow :: [Int] -> Int
calcBow x = (sum $ map (*2) $ delete (maximum x) x) + (foldr (*) 1 x)

e1, e2 :: Int
e1 = 58 + 43
e2 = 34 + 14
d2sol1 :: IO Int
d2sol1 = sol1 <$> input
d2sol2 :: IO Int
d2sol2 = sol2 <$> input
d2test1 :: Bool
d2test1 = sol1 example == e1
d2test2 :: Bool
d2test2 = sol2 example == e2
