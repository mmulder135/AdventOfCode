module Year2023.Solutions.Day02 () where
import Data.List.Utils (split)

exampleText :: [String]
exampleText = ["Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green","Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue","Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red","Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red","Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"]

example :: [Game]
example = [Game 1 [Blue 3, Red 4, Red 1, Green 2, Blue 6, Green 2], Game 2 [Blue 1, Green 2, Green 3, Blue 4, Red 1, Green 1, Blue 1]]

input :: IO [String]
input = lines <$> readFile "Year2023/Inputs/Day02.txt"

data Game = Game {gid ::Int, run :: Run}
        deriving Show
type Run = [Pull]
data Pull = Red Int | Green Int | Blue Int
        deriving Show
type Allowed = (Pull, Pull, Pull)
allw :: Allowed
allw = (Red 12, Green 13, Blue 14)

sol1 ::  [String] -> Int
sol1 = sum . map gid . filter (checkRun allw . run) . map parseGame

sol2 ::  [String] -> Int
sol2 = sum . map (getPowerGame . parseGame)

checkRun :: Allowed -> Run -> Bool
checkRun _ [] = True
checkRun a (x:xs)
            | checkPull x a = checkRun a xs
            | otherwise = False

checkPull :: Pull -> Allowed -> Bool
checkPull (Red i) (Red y, _ , _) = i<=y
checkPull (Green i) (_ , Green y , _) = i<=y
checkPull (Blue i) (_, _, Blue y) = i<=y
checkPull _ _ = error "Invalid input"

getPowerGame :: Game -> Int
getPowerGame game = r*g*b
    where
        (r, g, b) = getMax (0, 0, 0) $ run game

getMax :: (Int, Int, Int) -> Run -> (Int, Int, Int)
getMax res []                       = res
getMax res@(r, g, b) ((Red i):xs)   | i > r = getMax (i, g, b) xs
                                    | otherwise = getMax res xs
getMax res@(r, g, b) ((Green i):xs) | i > g = getMax (r, i, b) xs
                                    | otherwise = getMax res xs
getMax res@(r, g, b) ((Blue i):xs)  | i > b = getMax (r, g, i) xs
                                    | otherwise = getMax res xs

-- PARSING ---
-- "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
parseGame :: String -> Game
parseGame str = Game i $ parsePulls pullsStr
        where
            (game:pullsStr:_) = split ": " str
            i = read $ last $ split " " game

-- "3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
parsePulls :: String -> Run
parsePulls = map parsePull . concatMap (split ", ") . split "; "

parsePull :: String -> Pull
parsePull = parsePull' . split " "
        where
            parsePull' :: [String] -> Pull
            parsePull' (h:color:_)
                    | color == "red" = Red $ read h
                    | color == "green" = Green $ read h
                    | color == "blue" = Blue $ read h
                    | otherwise = error "Invalid Pull"