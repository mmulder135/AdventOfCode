module Year2015.Solutions.Day03
    (
    d3sol1,
    d3sol2,
    d3test1,
    d3test2
    )
where
import Data.Set

example :: String
example = "^v^v^v^v^v"
input :: IO String
input = readFile "Year2015/Inputs/Day03.txt"

sol1 :: String -> Int
sol1 = size . deliverPresent (0,0)

sol2 :: String -> Int
sol2 xs = size $ union (deliverPresent (0,0) $ santa xs) (deliverPresent (0,0) $ roboSanta xs)

deliverPresent :: (Int,Int) -> String -> Set (Int,Int)
deliverPresent cs [] = insert cs empty
deliverPresent (x,y) (p:ps)
    | p == '>' = insert (x,y) $ deliverPresent (x+1,y) ps
    | p == '<' = insert (x,y) $ deliverPresent (x-1,y) ps
    | p == '^' = insert (x,y) $ deliverPresent (x,y+1) ps
    | p == 'v' = insert (x,y) $ deliverPresent (x,y-1) ps

roboSanta :: String -> String
roboSanta (x:y:xs) = x : roboSanta xs
roboSanta _ = []

santa :: String -> String
santa (x:y:xs) = y : santa xs
santa _ = []

e1, e2 :: Int
e1 = 2
e2 = 11
d3sol1 :: IO Int
d3sol1 = sol1 <$> input
d3sol2 :: IO Int
d3sol2 = sol2 <$> input
d3test1 :: Bool
d3test1 = sol1 example == e1
d3test2 :: Bool
d3test2 = sol2 example == e2
