module Solutions.Day02
( d2sol1,
  d2sol2,
  d2test1,
  d2test2
) where

data Instr = Forward Int | Down Int | Up Int
  deriving (Show)

example :: [Instr]
example = [Forward 5, Down 5, Forward 8, Up 3, Down 8, Forward 2]

input :: IO [Instr]
input = parseInput <$> words <$> readFile "Inputs/Day02.txt"

sol1 :: [Instr] -> Int
sol1 x = move_hor x * move_dep x

sol2 :: [Instr] -> Int
sol2 x = h * d
  where (h,d) = calc_pos x

move_hor :: [Instr] -> Int
move_hor [] = 0
move_hor ((Forward z):zs) = z + move_hor zs
move_hor (x:zs) = move_hor zs

move_dep :: [Instr] -> Int
move_dep [] = 0
move_dep ((Down z):zs) = z + move_dep zs
move_dep ((Up z):zs) = (-z) + move_dep zs
move_dep (x:zs) = move_dep zs

move :: [Instr] -> (Int,Int) -> Int-> (Int, Int)
move [] x _ = x
move ((Down x):xs) pos aim = move xs pos (aim + x)
move ((Up x):xs) pos aim = move xs pos (aim - x)
move ((Forward x):xs) (hor,dep) aim = move xs (hor+x,dep+aim*x) aim

calc_pos :: [Instr] -> (Int,Int)
calc_pos x = move x (0,0) 0

-- Parsing

parseInput :: [String] -> [Instr]
parseInput [] = []
parseInput (x:y:zs)
    | l == 'f' = Forward (read y) : parseInput zs
    | l == 'd' = Down (read y) : parseInput zs
    | l == 'u' = Up (read y) : parseInput zs
      where l = head x

e1, e2 :: Int
e1 = 150
e2 = 900

d2sol1 :: IO Int
d2sol1 = sol1 <$> input
d2sol2 :: IO Int
d2sol2 = sol2 <$> input
d2test1 :: Bool
d2test1 = sol1 example == e1
d2test2 :: Bool
d2test2 = sol2 example == e2
