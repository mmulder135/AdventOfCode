module Year2015.Solutions.Day01
  (
    d1sol1,
    d1sol2,
    d1test1,
    d1test2
  )
where
example :: String
example = "))((((("
input :: IO String
input = readFile "Year2015/Inputs/Day01.txt"

sol1 :: String -> Int
sol1 [] = 0
sol1 (x:xs)
    | x == '(' = 1 + sol1 xs
    | x == ')' = -1 + sol1 xs

sol2 :: String -> Int
sol2 = search 0 0

search :: Int -> Int -> String -> Int
search (-1) z _ = z
search y z (x:xs)
    | x == '(' = search (y + 1) (z+1) xs
    | x == ')' = search (y - 1) (z+1) xs

e1, e2 :: Int
e1 = 3
e2 = 1

d1sol1 :: IO Int
d1sol1 = sol1 <$> input
d1sol2 :: IO Int
d1sol2 = sol2 <$> input
d1test1 :: Bool
d1test1 = sol1 example == e1
d1test2 :: Bool
d1test2 = sol2 example == e2
