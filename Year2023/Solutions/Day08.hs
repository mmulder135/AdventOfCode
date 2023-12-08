module Year2023.Solutions.Day08 where
import Data.List.Utils (split)
import Data.Map (Map, fromList, (!), keys)
import Data.Char (isAlpha)

example :: String
example = "RL\n\nAAA = (BBB, CCC)\nBBB = (DDD, EEE)\nCCC = (ZZZ, GGG)"
example2 :: String
example2 = "LLR\n\nAAA = (BBB, BBB)\nBBB = (AAA, ZZZ)\nZZZ = (ZZZ, ZZZ)"
example3 :: String
example3 = "LR\n\nYYA = (YYB, XXX)\nYYB = (XXX, YYZ)\nYYZ = (YYB, XXX)\nUUA = (UUB, XXX)\nUUB = (UUC, UUC)\nUUC = (UUZ, UUZ)\nUUZ = (UUB, UUB)\nXXX = (XXX, XXX)"

input :: IO String
input = readFile "Year2023/Inputs/Day08.txt"

type DesertMap = Map String (String, String)

sol1 :: String -> Int
sol1 = (\(instrs, map) -> follow (=="ZZZ") map instrs "AAA") . parse

sol2 :: String -> Int
sol2 input = foldl lcm 1 lenToZ
        where
            lenToZ = map (follow ((=='Z') . last) dMap instrs) $ startls dMap
            (instrs, dMap) = parse input
            startls :: DesertMap -> [String]
            startls m = filter ((=='A') . last) $ keys m 

follow :: (String -> Bool) -> DesertMap -> String -> String -> Int
follow endFun dMap instrs start = run start instrs
    where
        run :: String -> String -> Int
        run cur [] = run cur instrs
        run cur (i:ins) | endFun cur = 0
                        | otherwise = 1 + run (go i $ dMap ! cur) ins

go :: Char -> (String, String) -> String
go 'R' = snd
go 'L' = fst

------------------- PARSING -------------------
parse :: String -> (String, DesertMap)
parse input = (lr, parseMaps maps)
    where
        [lr, maps] = split "\n\n" input

parseMaps :: String -> DesertMap
parseMaps = fromList . map parseMap . split "\n"

parseMap :: String -> (String, (String, String))
parseMap = (\[from,to] -> (from, parseTo to)) . split " = "
    where
        parseTo :: String -> (String, String)
        parseTo = splitAt 3 . filter isAlpha
