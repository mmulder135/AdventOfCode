module Year2021.Solutions.Day08
    ( d8sol1,
      d8sol2,
      d8test1,
      d8test2
    ) where
import Data.List.Utils (split)
import Data.List ((\\),filter,map,sort,foldr)
import Data.Map (insert,Map,empty,lookup,fromList,toList)
import Data.Tuple (swap)

example :: String
example = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
-- exampleL :: IO [String]
-- exampleL = lines <$> readFile "Year2021/Inputs/Day08-example.txt"
input :: IO [String]
input = lines <$> readFile "Year2021/Inputs/Day08.txt"

sol1 :: [String] -> Int
sol1 = sum . map length . map (filter (\x -> x==2 || x==4 || x ==3 || x==7)) . map (map length) . map outputWords

sol2 :: [String] -> Int
sol2 = sum . map decode

outputWords :: String -> [String]
outputWords = words . last . split "|"

decode :: String -> Int
decode inp = read $ foldr (:) [] $ map inToOutput $ map sort $ map (translateInput dict) output
    where
      dict = fillMap inp
      output = outputWords inp

fillMap :: String -> Map Char Char
fillMap str =  fromList $ map (\(x,y) -> (head x, head y)) $ map swap $ filter (\(x,y) -> length x == 1) $ toList f'
    where
        inp = words $ head $ split "|" str
        cf' = insert "cf" (cf inp) empty
        a'  = insert "a" (a inp cf') cf'
        bd' = insert "bd" (bd inp cf') a'
        eg = eg' inp bd'
        g' = insert "g" (g eg) bd'
        e' = insert "e" (e eg g') g'
        d' = insert "d" (d inp e') e'
        b' = insert "b" (b d') d'
        c' = insert "c" (c inp b') b'
        f' = insert "f" (f c') c'

translateInput :: Map Char Char -> String -> String
translateInput _ [] = []
translateInput m (x:xs) =  lu x m : translateInput m xs

inToOutput :: String -> Char
inToOutput "abcefg" = '0'
inToOutput "cf" = '1'
inToOutput "acdeg" = '2'
inToOutput "acdfg" = '3'
inToOutput "bcdf" = '4'
inToOutput "abdfg" = '5'
inToOutput "abdefg" = '6'
inToOutput "acf" = '7'
inToOutput "abcdefg" = '8'
inToOutput "abcdfg" = '9'

-- Finding each letter only using the results of the functions defined above
cf :: [String] -> String
cf = head . filter (\x -> length x == 2)
a :: [String] -> Map String String -> String
a xs m = (head $ filter (\x -> length x == 3) xs) \\ (lu "cf" m)
bd :: [String] -> Map String String -> String
bd xs m = (head $ filter (\x -> length x == 4) xs) \\ (lu "cf" m)
eg' :: [String] -> Map String String -> [String]
eg' xs m = map (\\ lu "bd" m) $ map (\\ lu "cf" m) $ map (\\ lu "a" m) $ filter (\x -> length x == 5) xs
g :: [String] -> String
g = head . filter (\x -> length x == 1)
e :: [String] -> Map String String -> String
e eg m = (head $ filter (\x -> length x == 2) eg) \\ (lu "g" m)
d :: [String] -> Map String String -> String
d xs m = head $ filter (\x -> length x == 1) $ map (\\ lu "g" m) $ map (\\ lu "e" m) $ map (\\  lu "a" m) $ map (\\ lu "cf" m) $ filter (\x -> length x == 5) xs
b :: Map String String -> String
b m = lu "bd" m  \\  lu "d" m
c :: [String] -> Map String String -> String
c xs m = head $ map (\\ lu "e" m) $ filter (\x -> elem (head $ lu "e" m) x) $ map (\\ lu "g" m) $ map (\\ lu "bd" m) $ map (\\ lu "a" m) $ filter (\x -> length x == 5) xs
f :: Map String String -> String
f m = lu "cf" m \\ lu "c" m

-- Helper to do lookup and getting fromJust in one go
lu :: Ord a => a -> Map a b -> b
lu str m = fromJust $ Data.Map.lookup str m

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "Map not properly filled"
-- 0 : a b c   e f g : 6
-- 1 :     c     f   : 2
-- 2 : a   c d e   g : 5
-- 3 : a   c d   f g : 5
-- 4 :   b c d   f   : 4
-- 5 : a b   d   f g : 5
-- 6 : a b   d e f g : 6
-- 7 : a   c     f   : 3
-- 8 : a b c d e f g : 7
-- 9 : a b c d   f g : 6

-- -- Boilerplate
e1, e2 :: Int
e1 = 26
e2 = 61229
s1 = 352
s2 = 936117
d8sol1 :: IO Int
d8sol1 = sol1 <$> input
d8sol2 :: IO Int
d8sol2 = sol2 <$> input
d8test1 :: Bool
d8test1 = sol1 exampleL == e1
d8test2 :: Bool
d8test2 = sol2 exampleL == e2

exampleL :: [String]
exampleL = ["be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe","edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc","fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg","fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb","aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea","fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb","dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe","bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef","egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb","gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"]
