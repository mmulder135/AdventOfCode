{-# LANGUAGE TemplateHaskell #-}
module Year2022.Solutions.Day05
  (
    d5sol1,
    d5sol2
  )
where
import Test.QuickCheck
import Test.QuickCheck.All
import Data.List.Utils (split)
import Data.List (transpose)

type Stack = [Char]
data Instruction = Instruction  Int Int Int
  deriving (Show, Eq)

sol1 :: [String] -> String
sol1  = map head . runSolution doInstruction 

sol2 :: [String] -> String
sol2 = map head . runSolution doInstruction9001 

runSolution :: ([Stack] -> Instruction ->  [Stack]) -> [String] -> [Stack]
runSolution solver input = foldl solver stacks instructions
      where
        (stackText, instr) = splitStacksAndInstr input
        instructions = map parseInstruction instr
        stacks = parseStacks stackText

-- First char on stack is on top
doInstruction :: [Stack] -> Instruction ->  [Stack]
doInstruction stacks (Instruction count from to) = replace from newFrom $ replace to newTo stacks
      where
        oldFrom = stacks !! from
        oldTo = stacks !! to
        move = reverse $ take count oldFrom
        newFrom = drop count oldFrom
        newTo = move ++ oldTo

doInstruction9001 :: [Stack] -> Instruction ->  [Stack]
doInstruction9001 stacks (Instruction count from to) = replace from newFrom $ replace to newTo stacks
      where
        oldFrom = stacks !! from
        oldTo = stacks !! to
        move = take count oldFrom
        newFrom = drop count oldFrom
        newTo = move ++ oldTo

replace :: Int -> Stack -> [Stack] -> [Stack]
replace 0 s (x:xs) = s : xs
replace i s (x:xs) = x : replace (i-1) s xs
        

d5sol1 :: IO String
d5sol1 = sol1 <$> input
d5sol2 :: IO String
d5sol2 = sol2 <$> input

-- Parsing
splitStacksAndInstr :: [String] -> ([String], [String])
splitStacksAndInstr = splitStacksAndInstr' []
      where
        splitStacksAndInstr' :: [String] -> [String] -> ([String], [String])
        splitStacksAndInstr' ys ("":xs) = (ys, xs)
        splitStacksAndInstr' ys (x:xs) = splitStacksAndInstr' (x:ys) xs

parseInstruction :: String -> Instruction
parseInstruction line = Instruction count (from - 1) (to - 1) -- -1 for 0-indexed
      where
        [count, from, to]::[Int] = map read $ map ((words line) !!) [1,3,5]

parseStacks :: [String] -> [Stack]
parseStacks = concat . map words . transpose . getStackLetters . reverse . tail
    where
      getStackLetters :: [String] -> [String]
      getStackLetters = map (\line -> map (line !!) [1,5..length line])


-- Input
input :: IO [String]
input = lines <$> readFile "Year2022/Inputs/Day05.txt"

example :: [String]
example =
  [
    "    [D]    ",
    "[N] [C]    ",
    "[Z] [M] [P]",
    " 1   2   3 ",
    "",
    "move 1 from 2 to 1",
    "move 3 from 1 to 3",
    "move 2 from 2 to 1",
    "move 1 from 1 to 2"
  ]

-- Tests
prop_split = splitStacksAndInstr example == (
    [    
    " 1   2   3 ",
    "[Z] [M] [P]",
    "[N] [C]    ",
    "    [D]    "
    ], 
    [
    "move 1 from 2 to 1",
    "move 3 from 1 to 3",
    "move 2 from 2 to 1",
    "move 1 from 1 to 2"
    ])
prop_parsestacks = ["NZ","DCM","P"] == (parseStacks $ fst $ splitStacksAndInstr example)
prop_parseinstr = (map parseInstruction $ snd $ splitStacksAndInstr example) == [
      Instruction 1 1 0,
      Instruction 3 0 2,
      Instruction 2 1 0,
      Instruction 1 0 1
      ]
prop_doinstr = doInstruction stacks instr  == ["DNZ", "CM", "P"]
            where
              (stacksPre, instrsPre) = splitStacksAndInstr example
              stacks = parseStacks stacksPre
              instr = parseInstruction $ head instrsPre
prop_runsol = runSolution doInstruction example == ["C", "M", "ZNDP"]
prop_sol1 = sol1 example == "CMZ"
prop_sol2 = sol2 example == "MCD"


exampleStacks :: [Stack]
exampleStacks = parseStacks $ fst $ splitStacksAndInstr example
exampleInstrs :: [Instruction]
exampleInstrs = map parseInstruction $ snd $ splitStacksAndInstr example
-- QuickCheck
return []
check = $quickCheckAll
