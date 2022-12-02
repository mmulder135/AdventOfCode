{-# LANGUAGE TemplateHaskell #-}
module Year2022.Solutions.Day0
  (
    -- d0sol1,
    -- d0sol2
  )
where
import Test.QuickCheck
import Test.QuickCheck.All

data Instr = Rock | Paper | Scissors
    deriving (Show, Eq)
type Match = (Instr, Instr)

example :: [Match]
example = [(Rock, Paper), (Paper, Rock), (Scissors, Scissors)]

exampleText :: String
exampleText = "A Y\nB X\nC Z"

sol1 :: [Match] -> Int
sol1 = sum . map scoreMove

scoreMove :: Match -> Int
scoreMove x = scoreOwnMove x + scoreOutcome x

scoreOwnMove :: Match -> Int
scoreOwnMove (_, Rock) = 1
scoreOwnMove (_, Paper) = 2
scoreOwnMove (_, Scissors) = 3

scoreOutcome :: Match -> Int
scoreOutcome (t, y)
    | t == y = 3
    | t == Rock && y == Paper = 6
    | t == Paper && y == Scissors = 6
    | t == Scissors && y == Rock = 6
    | otherwise = 0

parse :: String -> [Match]
parse = map parseMatch . map words . lines

parseMatch :: [String] -> Match
parseMatch (t:o:xs) = (parseTheirs t, parseOwn o)

parseTheirs :: String -> Instr
parseTheirs x
    | x == "A" = Rock
    | x == "B" = Paper
    | x == "C" = Scissors
    | otherwise = error "Wrong input"

parseOwn :: String -> Instr
parseOwn x
    | x == "X" = Rock
    | x == "Y" = Paper
    | x == "Z" = Scissors
    | otherwise = error "Wrong input"

data Move = Win | Lose | Draw
    deriving (Show, Eq)
type Match2 = (Instr, Move)

parse2 :: String -> [Match]
parse2 = map match2ToMatch . map parseMatch2 . map words . lines

parseMove :: String -> Move
parseMove x
    | x == "X" = Lose
    | x == "Y" = Draw
    | x == "Z" = Win
    | otherwise = error "Wrong input"

parseMatch2 :: [String] -> Match2
parseMatch2 (t:o:xs) = (parseTheirs t, parseMove o)

match2ToMatch :: Match2 -> Match
match2ToMatch (t, Draw) = (t, t)
match2ToMatch (t, Win) 
      | t == Rock = (t, Paper)
      | t == Paper = (t, Scissors)
      | t == Scissors = (t, Rock)
match2ToMatch (t, Lose) 
      | t == Rock = (t, Scissors)
      | t == Paper = (t, Rock)
      | t == Scissors = (t, Paper)


d0sol1 :: IO Int
d0sol1 = sol1 <$> parse <$> input
d0sol2 :: IO Int
d0sol2 = sol1 <$> parse2 <$> input

prop_d2test1 = sol1 example == 15
prop_parse = example == parse exampleText
prop_d2test2 = (sol1 $ parse2 exampleText) == 12

input :: IO String
input = readFile "Year2022/Inputs/Day02.txt"

-- QuickCheck
return []
check = $quickCheckAll
