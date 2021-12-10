module Year2021.Solutions.Day10
    (
    d10sol1,
    d10sol2,
    d10test1,
    d10test2
    )
where
import Data.Filter (mapMaybe)
import Data.List (sort)

example :: [String]
example = ["[({(<(())[]>[[{[]{<()<>>","[(()[<>])]({[<{<<[]>>(","{([(<{}[<>[]}>{[]{[(<()>","(((({<>}<{<{<>}{[]{[]{}","[[<[([]))<([[{}[[()]]]","[{[{({}]{}}([{[{{{}}([]","{<[[]]>}<{[{[{[]{()[[[]","[<(<(<(<{}))><([]([]()","<{([([[(<>()){}]>(<<{{","<{([{{}}[<[[[<>{}]]]>[]]"]

input :: IO [String]
input = lines <$> readFile "Year2021/Inputs/Day10.txt"

sol1 :: [String] -> Int
sol1 = sum . map charScore . mapMaybe fst . map (flip parse [])
sol2 :: [String] -> Int
sol2 = middle . sort . map (foldl (\x y -> x * 5 + y) 0) . map (map compScore) . mapMaybe snd . map (flip parse [])

parse :: String -> [Char] -> (Maybe Char,Maybe [Char])
parse [] stack = (Nothing, Just stack)
parse (x:xs) [] = parse xs [x]
parse (x:xs) stack@(h:rest)
      | x `elem` opening = parse xs (x:stack)
      | x == matching h = parse xs rest
      | otherwise = (Just x, Nothing)

opening :: [Char]
opening = ['(','[','{','<']

matching :: Char -> Char
matching '(' = ')'
matching '[' = ']'
matching '{' = '}'
matching '<' = '>'

charScore :: Char -> Int
charScore ')' = 3
charScore ']' = 57
charScore '}' = 1197
charScore '>' = 25137

compScore :: Char -> Int
compScore '(' = 1
compScore '[' = 2
compScore '{' = 3
compScore '<' = 4

middle :: [a] -> a
middle xs = xs !! (length xs `div ` 2)

e1, e2 :: Int
e1 = 26397
e2 = 288957

d10sol1 :: IO Int
d10sol1 = sol1 <$> input
d10sol2 :: IO Int
d10sol2 = sol2 <$> input
d10test1 :: Bool
d10test1 = sol1 example == e1
d10test2 :: Bool
d10test2 = sol2 example == e2
