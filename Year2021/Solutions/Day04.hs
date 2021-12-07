module Year2021.Solutions.Day04
( d4sol1,
  d4sol2,
  d4test1,
  d4test2
) where
  -- example = lines <$> readFile "Inputs/Day04-example.txt"
example ::[String]
example = ["7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1","","22 13 17 11  0"," 8  2 23  4 24","21  9 14 16  7"," 6 10  3 18  5"," 1 12 20 15 19",""," 3 15  0  2 22"," 9 18 13 17  5","19  8  7 25 23","20 11 10 24  4","14 21 16 12  6","","14 21 17 24  4","10 16 15  9 19","18  8 23 26 20","22 11 13  6  5"," 2  0 12  3  7"]

input :: IO [String]
input = lines <$> readFile "Year2021/Inputs/Day04.txt"

sol1 :: [String] -> Int
sol1 x = score card wNum
    where
      card = getWinningCard result
      wNum = read (last used) :: Int
      used = filter (`notElem` left) nums
      (result,left) = run cards nums
      cards = getCards x
      nums = getNumbers x

sol2 :: [String] -> Int
sol2 x = score card wNum
    where
      wNum = read (last used) :: Int
      used = filter (`notElem` left) nums
      (card,left) = run2 cards nums
      cards = getCards x
      nums = getNumbers x

run2 :: [BingoCard] -> [String] -> (BingoCard, [String])
run2 [b] xs = (card, resNums)
      where
        card = head resCards
        (resCards, resNums) = run [b] xs
run2 bs xs = run2 left resNums
      where
        left = getLosingCards resCards
        (resCards, resNums) = run bs xs

getLosingCards :: [BingoCard] -> [BingoCard]
getLosingCards b =  filter (\x -> not (bingo x)) b

getNumbers :: [String] -> [String]
getNumbers x = foldr f [[]] (head x)
      where f c l@(x:xs) | c == ',' = []:l
                         | otherwise = (c:x):xs

type BingoCard = [[String]]

getCards :: [String] -> [BingoCard]
getCards [] = []
getCards [x] = []
getCards (x:y:xs) = getCards' (xs)

getCards' :: [String] -> [BingoCard]
getCards' [] = []
getCards' xs = b : getCards' ys
        where
          (b, ys) = getCard ([],xs)

getCard :: (BingoCard, [String])  -> (BingoCard, [String])
getCard (b,[]) = (b,[])
getCard (b, x:xs)
  | x == "" = (b,xs)
  | otherwise = getCard ((words x) : b , xs)

run :: [BingoCard] -> [String] -> ([BingoCard],[String])
run cards (x:xs)
    | elem True (map bingo cards) = (cards,(x:xs))
    | otherwise = run (map (replaceNumber x) cards) xs

getWinningCard :: [BingoCard] -> BingoCard
getWinningCard b = head ( filter (\x -> bingo x) b)

score :: BingoCard -> Int -> Int
score c x = (sumCard c) * x

sumCard :: BingoCard -> Int
sumCard [] = 0
sumCard (x:xs) = sum nums + sumCard xs
  where
    fil = filter (\y -> y /= "") x
    nums = map (read :: String -> Int) fil


replaceNumber :: String -> BingoCard -> BingoCard
replaceNumber _ [] = []
replaceNumber y (x:xs) =( replace x y ): replaceNumber y xs

replace :: [String] -> String -> [String]
replace [] _ = []
replace (x:xs) y
    | x == y = "" : replace xs y
    | otherwise = x : replace xs y

bingo :: BingoCard -> Bool
bingo b
    | verticalBingo b = True
    | horizontalBingo b = True
    | otherwise = False

verticalBingo :: BingoCard -> Bool
verticalBingo b = horizontalBingo (transpose b)

horizontalBingo :: BingoCard -> Bool
horizontalBingo [] = False
horizontalBingo (x:xs)
      | length (filter (\y -> y /= "") x) == 0 = True
      | otherwise = horizontalBingo xs

transpose :: BingoCard -> BingoCard
transpose [] = []
transpose ([]:xs) = []
transpose x = (map head x) : (transpose (map tail x))

e1, e2 :: Int
e1 = 4512
e2 = 1924

d4sol1 :: IO Int
d4sol1 = sol1 <$> input
d4sol2 :: IO Int
d4sol2 = sol2 <$> input
d4test1 :: Bool
d4test1 = sol1 example == e1
d4test2 :: Bool
d4test2 = sol2 example == e2
