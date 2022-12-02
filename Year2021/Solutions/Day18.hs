{-# LANGUAGE TemplateHaskell #-}
module Year2021.Solutions.Day18
  (
    d18sol1,
    d18sol2
  )
where
import Test.QuickCheck
import Test.QuickCheck.All
import Text.ParserCombinators.ReadP
import Data.Char (isDigit)
import Data.Maybe (fromJust)
import Control.Applicative

data Tree = Leaf Int | Pair Tree Tree deriving (Eq)

instance Show Tree where
  show (Leaf n) = show n
  show (Pair l r) = show [l, r]

instance Read Tree where
  readsPrec _ = readP_to_S parseTree

sol1 :: [Tree] -> Int
sol1 = magnitude . calc

sol2 :: [Tree] -> Int
sol2 ts = maximum $ [magnitude . reduce $ Pair x y | x <- ts, y <- ts, x /= y]

calc :: [Tree] -> Tree
calc = foldl1 (\a b -> reduce $ Pair a b)

magnitude :: Tree -> Int
magnitude (Pair l r) = (3 * (magnitude l)) + (2*(magnitude r))
magnitude (Leaf i) = i

reduce :: Tree -> Tree
reduce = fromJust . reduce'

reduce' :: Tree -> Maybe Tree
reduce' p = (explode p >>= reduce') <|> (split p >>= reduce') <|> Just p

split :: Tree -> Maybe Tree
split (Leaf n)
  | n > 9 = Just (Pair (Leaf $ n `div` 2) (Leaf $ succ n `div` 2))
  | otherwise = Nothing
split (Pair l r) = flip Pair r <$> split l <|> Pair l <$> split r

explode :: Tree -> Maybe Tree
explode t = fst <$> explode' 0 t

explode' :: Int -> Tree -> Maybe (Tree, (Int, Int))
explode' _ (Leaf _) = Nothing
explode' 4 (Pair (Leaf l) (Leaf r)) = Just (Leaf 0, (l,r))
explode' d (Pair l r) = left <$> explode' (d + 1) l <|> right <$> explode' (d + 1) r
                where
                  left (t, (ln, rn)) = (Pair t (addl rn r), (ln, 0))
                  right (t, (ln, rn)) = (Pair (addr ln l) t, (0, rn))
                  
                  addl :: Int -> Tree -> Tree
                  addl x (Leaf i) = Leaf (x + i)
                  addl x (Pair l r) = Pair (addl x l) r
                  addr :: Int -> Tree -> Tree
                  addr x (Leaf i) = Leaf (x + i)
                  addr x (Pair l r) = Pair l (addr x r)

-- Parsing
parseTree :: ReadP Tree
parseTree = leafParser +++ pairParser

leafParser :: ReadP Tree
leafParser = Leaf . read <$> many1 (satisfy isDigit)
pairParser :: ReadP Tree
pairParser = Pair <$ char '[' <*> parseTree <* char ',' <*> parseTree <* char ']'

input :: IO [Tree]
input = map read <$> lines <$> readFile "Year2021/Inputs/Day18.txt"
example, example2 :: [Tree]
example = map read ["[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]","[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]","[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]","[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]","[7,[5,[[3,8],[1,4]]]]","[[2,[2,2]],[8,[8,1]]]","[2,9]","[1,[[[9,3],9],[[9,0],[0,7]]]]","[[[5,[7,4]],7],1]","[[[[4,2],2],6],[8,7]]"]
example2 = map read ["[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]","[[[5,[2,8]],4],[5,[[9,9],0]]]","[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]","[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]","[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]","[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]","[[[[5,4],[7,7]],8],[[8,3],8]]","[[9,3],[[9,9],[6,[4,9]]]]","[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]","[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"]

-- Solutions
d18sol1 :: IO Int
d18sol1 = sol1 <$> input
d18sol2 :: IO Int
d18sol2 = sol2 <$> input


-- props
prop_parse = parse1 && parse2 && parse3 && parse4
parse1 = (Pair (Leaf 1) (Leaf 2)) == read "[1,2]"
parse2 = (Pair (Pair (Leaf 3) (Leaf 4)) (Leaf 5)) == read "[[3,4],5]"
parse3 = (Pair (Pair (Leaf 1) (Leaf 9)) (Pair (Leaf 8) (Leaf 5))) == read "[[1,9],[8,5]]"
parse4 = (Pair (Pair (Pair (Pair (Pair (Leaf 9) (Leaf 8)) (Leaf 1)) (Leaf 2)) (Leaf 3)) (Leaf 4)) == read "[[[[[9,8],1],2],3],4]"

prop_split = split1 && split2
split1 = Nothing == split (read "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")
split2 = Just (read "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]") == split (read "[[[[0,7],4],[15,[0,13]]],[1,1]]")

prop_explode = explode1 && explode2 && explode3 && explode4 && explode5 && explode6
explode1 = (Just (read "[[[[0,9],2],3],4]")) == explode (read "[[[[[9,8],1],2],3],4]")
explode2 = (Just (read "[7,[6,[5,[7,0]]]]")) == explode (read "[7,[6,[5,[4,[3,2]]]]]")
explode3 = (Just (read "[[6,[5,[7,0]]],3]")) == explode (read "[[6,[5,[4,[3,2]]]],1]")
explode4 = (Just (read "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")) == explode (read "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]")
explode5 = (Just (read "[[3,[2,[8,0]]],[9,[5,[7,0]]]]")) == explode (read "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
explode6 = Nothing == explode (read "[[[[0,7],4],[15,[0,13]]],[1,1]]")

prop_reduce = (read "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]") == reduce (read "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")

prop_calc = calc1 && calc2
calc1 = read "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]" == calc example
calc2 = read "[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]" == calc example2

prop_sol1 = 4140 == sol1 example2
prop_sol2 = 3993 == sol2 example2
-- QuickCheck
return []
check = $quickCheckAll
