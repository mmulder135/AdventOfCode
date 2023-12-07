module Year2023.Solutions.Day06 where

type Race = (Int, Int)

example :: [Race]
example = [(7,9), (15,40), (30,200)]
example2:: Race
example2 = (71530,940200)

input :: [Race]
input = [(40,277), (82,1338),(91,1349),(66,1063)]
input2 :: Race
input2 = (40829166,277133813491063)

sol1 :: [Race] -> Int
sol1 = product . map getOptions

-- Bruteforce approach, better approach would be solving the equation, but this finished fast enough for now
sol2 :: Race -> Int
sol2 = getOptions

getOptions :: (Int,Int) -> Int
getOptions (time, dist) = length $ filter (> dist) $ getDists time

getDists :: Int -> [Int]
getDists time = map (getDist time) [1..time-1]

getDist :: Int -> Int -> Int
getDist time push = push * (time - push)