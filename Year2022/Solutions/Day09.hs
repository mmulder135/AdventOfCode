{-# LANGUAGE TemplateHaskell #-}
module Year2022.Solutions.Day09
  (
    d9sol1,
    d9sol2
  )
where
import Test.QuickCheck
import Test.QuickCheck.All
import qualified Data.Set as Set     (Set, union, fromList, empty, size)

data Instruction = U  | R  | D  | L 
    deriving (Show, Eq)
type Coord = (Int, Int) -- (x, y)
type State = (Coord, Coord) -- (head, tail)
type Rope = [Coord] -- [H, 1, 2, 3, 4, 5, 6, 7, 8, 9]

sol1 :: [String] -> Int
sol1 =  Set.size . Set.fromList . map snd . scanl doInstruction ((0,0), (0,0)) . parse

sol2 :: [String] -> Int
sol2 =  Set.size . Set.fromList . map last . scanl doInstructionRope (replicate 10 (0,0)) . parse

doInstructionRope :: Rope -> Instruction -> Rope
doInstructionRope (head : rope) instr = scanl1 (curry updateTail) ((updateHead head instr) : rope)

doInstruction :: State -> Instruction -> State
doInstruction (head, tail) instr = (newHead, newTail)
      where
        newTail = updateTail (newHead, tail)
        newHead = updateHead head instr

updateHead :: Coord -> Instruction -> Coord
updateHead (xh, yh) (U) = (xh, yh + 1)
updateHead (xh, yh) (R) = (xh + 1, yh)
updateHead (xh, yh) (D) = (xh, yh - 1)
updateHead (xh, yh) (L) = (xh - 1, yh)

updateTail :: State -> Coord
updateTail start@((xh, yh), tail@(xt, yt)) 
      | touching xh xt, touching yh yt = tail
      | otherwise = (move xh xt, move yh yt)
      where
        move :: Int -> Int -> Int
        move xh xt = signum (xh - xt) + xt

        touching :: Int -> Int -> Bool
        touching xh xt = abs (xh - xt) < 2

-- Parsing
parse :: [String] -> [Instruction]
parse = concatMap parseLine

parseLine :: String -> [Instruction]
parseLine ('U':i) = replicate (read i) U
parseLine ('R':i) = replicate (read i) R
parseLine ('D':i) = replicate (read i) D
parseLine ('L':i) = replicate (read i) L

d9sol1 :: IO Int
d9sol1 = sol1 <$> input
d9sol2 :: IO Int
d9sol2 = sol2 <$> input

prop_updatetail1    = updateTail ((0,0), (0,0))   == (0,0)
prop_updatetailh1   = updateTail ((1,0), (0,0))   == (0,0)
prop_updatetailh2   = updateTail ((2,0), (0,0))   == (1,0)
prop_updatetailhn1  = updateTail ((-1,0), (0,0))  == (0,0)
prop_updatetailhn2  = updateTail ((-2,0), (0,0))  == (-1,0)
prop_updatetailv1   = updateTail ((0,1), (0,0))   == (0,0)
prop_updatetailv2   = updateTail ((0,2), (0,0))   == (0,1)
prop_updatetailvn1  = updateTail ((0,-1), (0,0))  == (0,0)
prop_updatetailvn2  = updateTail ((0,-2), (0,0))  == (0,-1)
prop_updatetaild1   = updateTail ((1,1), (0,0))   == (0,0)
prop_updatetaild2   = updateTail ((1,2), (0,0))   == (1,1)
prop_updatetaildn2  = updateTail ((-1,-2), (0,0)) == (-1,-1)

prop_sol1           = sol1 example                == 13
prop_sol21          = sol2 example                == 1
prop_sol22          = sol2 example2               == 36


example :: [String]
example =[
    "R 4",
    "U 4",
    "L 3",
    "D 1",
    "R 4",
    "D 1",
    "L 5",
    "R 2"
  ]
example2 :: [String]
example2 = [
    "R 5",
    "U 8",
    "L 8",
    "D 3",
    "R 17",
    "D 10",
    "L 25",
    "U 20"
    ]

input :: IO [String]
input = lines <$> readFile "Year2022/Inputs/Day09.txt"

-- QuickCheck
return []
check = $quickCheckAll
