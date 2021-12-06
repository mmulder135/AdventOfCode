module Solutions.Day01
( d1sol1,
  d1sol2,
  d1test1,
  d1test2
)
where

example :: [Int]
example = [199,200,208,210,200,207,240,269,260,263]

input :: IO [Int]
input = (map read) <$> words <$> readFile "Inputs/Day01.txt"

sol1 :: [Int] -> Int
sol1 = cntLarger

sol2 :: [Int] -> Int
sol2 = sol1 . sliding

cntLarger :: [Int] -> Int
cntLarger (x:y:ys)
  | y > x = 1 + cntLarger (y:ys)
  | otherwise = cntLarger (y:ys)
cntLarger _ = 0

sliding :: [Int] -> [Int]
sliding (x:y:z:xs) = (x+y+z) : sliding (y:z:xs)
sliding _ = []

e1, e2 :: Int
e1 = 7
e2 = 5

d1sol1, d1sol2 :: IO Int
d1sol1 = sol1 <$> input
d1sol2 = sol2 <$> input
d1test1, d1test2 :: Bool
d1test1 = sol1 example == e1
d1test2 = sol2 example == e2
