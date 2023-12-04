module Year2023.Solutions.Day04 where
import Data.List (sort, intersect)
import Data.List.Utils (split)
import Data.String.Utils (strip)
import Data.Map (Map)
import qualified Data.Map as M

example :: [String]
example =["Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53", "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19", "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1", "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83", "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36", "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"]

input :: IO [String]
input = lines <$> readFile "Year2023/Inputs/Day04.txt"

data Card = Card {winning :: [Int], own :: [Int]} deriving Show
makeCard :: [[Int]] -> Card
makeCard (x:y:_) = Card x y

parseCard :: String -> Card
parseCard = makeCard . map (sort . map read . words) . split "|" . last . split ":"

getWinning :: Card -> [Int]
getWinning card = winning card `intersect` own card

scoreCard :: Card -> Int
scoreCard card  | amount > 0    = 2 ^  (amount - 1)
                | otherwise     = 0
                where
                    amount = length $ getWinning card

sol1 :: [String] -> Int
sol1 = sum . map (scoreCard . parseCard)

-- -- NAIVE APPROACH ---
-- sol2 :: [String] -> Int
-- sol2 input = sum $ map (extend dict) wonList
--     where
--         wonList = zip [1..] $ map (length . getWinning . parseCard) input
--         dict = M.fromList wonList

-- extend :: Map Int Int -> (Int, Int) -> Int
-- extend _ (_,0) = 1
-- extend dict (i,a) = 1 + sum (zipWith (curry (extend dict)) wonCards (map (dict M.!) wonCards))
--     where
--         wonCards = take a [i+1..]
--         wonList = zip wonCards $ map (dict M.!) wonCards

sol2 ::[String] -> Int
sol2 input = sum $ M.elems $ foldr (cardGives dict) M.empty [1..M.size dict]
        where
            wonList = zip [1..] $ map (length . getWinning . parseCard) input
            dict = M.fromList wonList

cardGives :: Map Int Int -> Int -> Map Int Int -> Map Int Int
cardGives dict i res | dict M.! i == 0  = M.insert i 1 res
                     | otherwise        = M.insert i wonAmount res
                where
                    wonCards = take (dict M.! i) [i+1..]
                    wonAmount = 1 + sum (map (res M.!) wonCards)
