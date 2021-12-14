{-# LANGUAGE TemplateHaskell #-}
module Year2021.Solutions.Day14
  (
    d14sol1,
    d14sol2
  )
where
import Test.QuickCheck
import Test.QuickCheck.All
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (group, sort, tails)
import Data.List.Utils (split)
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS


sol1 :: (String,Map String Char) -> Int
sol1 x = maximum quantities - minimum quantities
  where
    res = runNaive x 10
    quantities = map length $ group $ sort res

sol2 :: (String,Map String Char) -> Int
sol2 (str, m) = maximum ls - minimum ls
  where
    inputSet = MS.fromList $ filter (\x -> length x == 2) $  map (take 2) $ tails str
    resultSet = runInsertion m inputSet 40
    singletonSet = MS.concatMap (\x->x) resultSet
    ls = map (\(a,b) -> b `quot` 2 + b `mod` 2) $ MS.toOccurList singletonSet

runInsertion :: Map String Char -> MultiSet String -> Int -> MultiSet String
runInsertion _ ms 0 = ms
runInsertion m ms i = runInsertion m (insertion ms m) (i-1)

insertion :: MultiSet String -> Map String Char -> MultiSet String
insertion ms m = MS.concatMap (\[a,b] -> let c = m Map.! [a,b] in [[a,c],[c,b]]) ms

runNaive :: (String, Map String Char) -> Int -> String
runNaive (str,_) 0 = str
runNaive (str,m) i = runNaive ((naive m str),m) (i-1)

naive :: Map String Char -> String -> String
naive m (x:y:ys) = x : (m Map.! [x,y]) : naive m (y:ys)
naive _ x = x

input :: IO (String,Map String Char)
input = parse <$> lines <$> readFile "Year2021/Inputs/Day14.txt"

example :: (String,Map String Char)
example = ("NNCB", Map.fromList [("CH", 'B'),("HH", 'N'),("CB", 'H'),("NH", 'C'),("HB", 'C'),("HC", 'B'),("HN", 'C'),("NN", 'C'),("BH", 'H'),("NC", 'B'),("NB", 'B'),("BN", 'B'),("BB", 'N'),("BC", 'B'),("CC", 'N'),("CN", 'C')])

parse :: [String] -> (String,Map String Char)
parse xs = (head str,m)
      where
        [str,instr] = split [""] xs
        m = Map.fromList $ map (\[x,y] -> (x, head y)) $ map (split " -> ") instr

d14sol1 :: IO Int
d14sol1 = sol1 <$> input
d14sol2 :: IO Int
d14sol2 = sol2 <$> input

prop_naive1 = "NCNBCHB" == naive (snd example) (fst example)
prop_naive2 = "NBCCNBBBCBHCB" == naive (snd example) (naive (snd example) (fst example))
prop_runnaive1 = "NCNBCHB" == runNaive example 1
prop_runnaive2 = "NBCCNBBBCBHCB" == runNaive example 2
prop_runnaive3 = "NBBBCNCCNBBNBNBBCHBHHBCHB" == runNaive example 3
prop_runnaive4 = "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB" == runNaive example 4
prop_sol1 = 1588 == sol1 example
prop_parse = example == parse exampleP
exampleP = ["NNCB","","CH -> B","HH -> N","CB -> H","NH -> C","HB -> C","HC -> B","HN -> C","NN -> C","BH -> H","NC -> B","NB -> B","BN -> B","BB -> N","BC -> B","CC -> N","CN -> C"]
prop_sol2 = 2188189693529 == sol2 example

-- QuickCheck
return []
check = $quickCheckAll
