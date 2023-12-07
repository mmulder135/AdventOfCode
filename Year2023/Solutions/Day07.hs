module Year2023.Solutions.Day07 where
import Data.List (group, sort, sortBy)
import Data.Ord ( Down(Down), comparing )

example:: String
example = "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483"
input :: IO String
input = readFile "Year2023/Inputs/Day07.txt"

data Hand = Hand {cards :: [Int], bid :: Int} deriving Show
type ScoredHand = (Int, Hand)

sol1 :: String -> Int
sol1 = sum . zipWith (\i h -> i * bid h) [1..] . sortHands . parse

sol2 :: String -> Int -- inserted sortHands and getScoredHands to avoid making new functions
sol2 = sum . zipWith (\i h -> i * bid h) [1..] . map snd . sortBy compareScoredHands . map (\ x -> (scoreHand2 x, x)) . parse2

sortHands :: [Hand] -> [Hand]
sortHands = map snd . sortBy compareScoredHands . getScoredHands

compareScoredHands :: ScoredHand -> ScoredHand -> Ordering
compareScoredHands (i1,h1) (i2,h2)  | i1 /= i2 = compare i1 i2
                                    | otherwise = compare (cards h1) (cards h2)

getScoredHands :: [Hand] -> [ScoredHand]
getScoredHands = map (\ x -> (scoreHand x, x))

scoreHand :: Hand -> Int
scoreHand = scoreGroup . sortBy (comparing Down) . map length . group . sort . cards
    where
        scoreGroup :: [Int] -> Int
        scoreGroup [5] = 7
        scoreGroup [4,1] = 6
        scoreGroup [3,2] = 5
        scoreGroup [3,1,1] = 4
        scoreGroup [2,2,1] = 3
        scoreGroup [2,1,1,1] = 2
        scoreGroup [1,1,1,1,1] = 1

scoreHand2 :: Hand -> Int
scoreHand2 hand | null filtered     = scoreHand hand
                | otherwise         = scoreHand $ replaceInHand 1 replaceFor hand
                where
                    filtered = filter (/=1) $ cards hand
                    replaceFor = mostCommon filtered


mostCommon :: [Int] -> Int
mostCommon = snd . maximum . map (\xs -> (length xs, head xs)) . group . sort

------ PARSING ---------
parse2 :: String -> [Hand]
parse2 = map (replaceInHand 11 1) . parse

replaceInHand :: Int -> Int -> Hand -> Hand
replaceInHand f r (Hand c b) = Hand (replace f r c) b

replace :: Int -> Int -> [Int] -> [Int]
replace f r = map (\x -> if x == f then r else x)

parse :: String -> [Hand]
parse = map (parseLine . words) . lines

parseLine :: [String] -> Hand
parseLine [cards,bid] = Hand (map getCardValue cards) $ read bid
parseLine _ = error "Invalid line"

getCardValue :: Char -> Int
getCardValue 'A' = 14
getCardValue 'K' = 13
getCardValue 'Q' = 12
getCardValue 'J' = 11
getCardValue 'T' = 10
getCardValue x = read [x]

