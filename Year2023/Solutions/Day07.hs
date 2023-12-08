module Year2023.Solutions.Day07 where
import Data.List (group, sort, sortBy)
import Data.Ord ( Down(Down), comparing )

example:: String
example = "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483"
input :: IO String
input = readFile "Year2023/Inputs/Day07.txt"

data Hand = Hand {cards :: [Int], bid :: Int, score :: Int} deriving (Show, Eq)

instance Ord Hand where
    (Hand c1 b1 s1) `compare` (Hand c2 b2 s2)
            | s1 /= s2  = s1 `compare` s2
            | otherwise = c1 `compare` c2

sol1 :: String -> Int
sol1 = sum . zipWith (\i h -> i * bid h) [1..] . sort . getScoredHands . parse

sol2 :: String -> Int -- inserted getScoredHands to avoid making new functions
sol2 = sum . zipWith (\i h -> i * bid h) [1..] . sort . map (\(Hand c b s) -> Hand c b $ scoreCards2 c). parse2

getScoredHands :: [Hand] -> [Hand]
getScoredHands = map (\(Hand c b s) -> Hand c b $ scoreCards c)

scoreCards :: [Int] -> Int
scoreCards = scoreGroup . sortBy (comparing Down) . map length . group . sort
    where
        scoreGroup :: [Int] -> Int
        scoreGroup [5] = 7
        scoreGroup [4,1] = 6
        scoreGroup [3,2] = 5
        scoreGroup [3,1,1] = 4
        scoreGroup [2,2,1] = 3
        scoreGroup [2,1,1,1] = 2
        scoreGroup [1,1,1,1,1] = 1

scoreCards2 :: [Int] -> Int
scoreCards2 c   | null filtered     = scoreCards c
                | otherwise         = scoreCards $ replace 1 replaceFor c
                where
                    filtered = filter (/=1) c
                    replaceFor = mostCommon filtered

mostCommon :: [Int] -> Int
mostCommon = snd . maximum . map (\xs -> (length xs, head xs)) . group . sort

------ PARSING ---------
parse2 :: String -> [Hand]
parse2 = map (replaceInHand 11 1) . parse

replaceInHand :: Int -> Int -> Hand -> Hand
replaceInHand f r (Hand c b s) = Hand (replace f r c) b s

replace :: Int -> Int -> [Int] -> [Int]
replace f r = map (\x -> if x == f then r else x)

parse :: String -> [Hand]
parse = map (parseLine . words) . lines

parseLine :: [String] -> Hand
parseLine [cards,bid] = Hand (map getCardValue cards) (read bid) 0
parseLine _ = error "Invalid line"

getCardValue :: Char -> Int
getCardValue 'A' = 14
getCardValue 'K' = 13
getCardValue 'Q' = 12
getCardValue 'J' = 11
getCardValue 'T' = 10
getCardValue x = read [x]