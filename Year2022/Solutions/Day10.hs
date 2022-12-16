{-# LANGUAGE TemplateHaskell #-}
module Year2022.Solutions.Day10
  (
    d10sol1,
    d10sol2
  )
where
import Test.QuickCheck
import Test.QuickCheck.All
import Data.String.Utils (startswith)

data Instruction = Noop | Addx Int
    deriving (Show, Eq)


data Screen = Screen [Char]
instance Show Screen where
  show (Screen []) = ""
  show (Screen xs) = (take 40 xs) ++ "\n" ++ (show (Screen (drop 40 xs)))

sol1 :: [String] -> Int
sol1 = sum . getScores . getCPUSignals . parse

getScores :: [Int] -> [Int]
getScores = map (uncurry (*)) . filter ((== 20) . (`mod` 40) . fst) . zip [1..] 

sol2 :: [String] -> Screen
sol2 = Screen . getCRT . getCPUSignals . parse

getCRT :: [Int] -> [Char]
getCRT reg = map (getPixel reg) [0..239] 

getPixel :: [Int] -> Int -> Char
getPixel reg i 
      | i < 1                                     = '#'
      | abs (drawIndex - reg !! cycleIndex) <= 1  = '#'
      | otherwise                                 = '.'
      where
        cycleIndex = i - 1
        drawIndex = i `mod` 40



getCPUSignals :: [Instruction] -> [Int]
getCPUSignals = run 1

run :: Int -> [Instruction]  -> [Int]
run _ []            = []
run c (Noop:xs)     = c : run c xs
run c ((Addx x):xs) = c : (c + x) : run (c + x) xs 

parse :: [String] -> [Instruction]
parse = map parseInstruction

parseInstruction :: String -> Instruction
parseInstruction str 
      | str == "noop"         = Noop
      | startswith "addx" str = Addx $ read $ last $ words str
      | otherwise             = error ("Invalid str: " ++ str)

d10sol1 :: IO Int
d10sol1 = sol1 <$> input
d10sol2 :: IO Screen
d10sol2 = sol2 <$> input

input :: IO [String]
input = lines <$> readFile "Year2022/Inputs/Day10.txt"


e :: [String]
e = ["noop","addx 3","addx -5"]
example :: [String]
example = [
    "addx 15",
    "addx -11",
    "addx 6",
    "addx -3",
    "addx 5",
    "addx -1",
    "addx -8",
    "addx 13",
    "addx 4",
    "noop",
    "addx -1",
    "addx 5",
    "addx -1",
    "addx 5",
    "addx -1",
    "addx 5",
    "addx -1",
    "addx 5",
    "addx -1",
    "addx -35",
    "addx 1",
    "addx 24",
    "addx -19",
    "addx 1",
    "addx 16",
    "addx -11",
    "noop",
    "noop",
    "addx 21",
    "addx -15",
    "noop",
    "noop",
    "addx -3",
    "addx 9",
    "addx 1",
    "addx -3",
    "addx 8",
    "addx 1",
    "addx 5",
    "noop",
    "noop",
    "noop",
    "noop",
    "noop",
    "addx -36",
    "noop",
    "addx 1",
    "addx 7",
    "noop",
    "noop",
    "noop",
    "addx 2",
    "addx 6",
    "noop",
    "noop",
    "noop",
    "noop",
    "noop",
    "addx 1",
    "noop",
    "noop",
    "addx 7",
    "addx 1",
    "noop",
    "addx -13",
    "addx 13",
    "addx 7",
    "noop",
    "addx 1",
    "addx -33",
    "noop",
    "noop",
    "noop",
    "addx 2",
    "noop",
    "noop",
    "noop",
    "addx 8",
    "noop",
    "addx -1",
    "addx 2",
    "addx 1",
    "noop",
    "addx 17",
    "addx -9",
    "addx 1",
    "addx 1",
    "addx -3",
    "addx 11",
    "noop",
    "noop",
    "addx 1",
    "noop",
    "addx 1",
    "noop",
    "noop",
    "addx -13",
    "addx -19",
    "addx 1",
    "addx 3",
    "addx 26",
    "addx -30",
    "addx 12",
    "addx -1",
    "addx 3",
    "addx 1",
    "noop",
    "noop",
    "noop",
    "addx -9",
    "addx 18",
    "addx 1",
    "addx 2",
    "noop",
    "noop",
    "addx 9",
    "noop",
    "noop",
    "noop",
    "addx -1",
    "addx 2",
    "addx -37",
    "addx 1",
    "addx 3",
    "noop",
    "addx 15",
    "addx -21",
    "addx 22",
    "addx -6",
    "addx 1",
    "noop",
    "addx 2",
    "addx 1",
    "noop",
    "addx -10",
    "noop",
    "noop",
    "addx 20",
    "addx 1",
    "addx 2",
    "addx 2",
    "addx -6",
    "addx -11",
    "noop",
    "noop",
    "noop"
  ]

-- example = lines <$> readFile "Year2022/Inputs/Day10-example.txt"
prop_getVals =  (getScores $ getCPUSignals $ parse example) == [420, 1140, 1800, 2940, 2880, 3960]
prop_sol1 = sol1 example == 13140
prop_CRT1 = "##..##..##..##..##..##..##..##..##..##.." == (take 40 $ getCRT $ getCPUSignals $ parse example)
prop_CRT2 = "###...###...###...###...###...###...###." == (take 40 $ drop 40 $ getCRT $ getCPUSignals $ parse example)
prop_CRT3 = "####....####....####....####....####...." == (take 40 $ drop 80 $ getCRT $ getCPUSignals $ parse example)
prop_CRT4 = "#####.....#####.....#####.....#####....." == (take 40 $ drop 120 $ getCRT $ getCPUSignals $ parse example)
prop_CRT5 = "######......######......######......####" == (take 40 $ drop 160 $ getCRT $ getCPUSignals $ parse example)
prop_CRT6 = "#######.......#######.......#######....." == (take 40 $ drop 200 $ getCRT $ getCPUSignals $ parse example)
-- QuickCheck
return []
check = $quickCheckAll
