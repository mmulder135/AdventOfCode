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

type Rules = Map String Char
type Polymer = MultiSet String

sol1 :: (Polymer, Rules) -> Int
sol1 (polymer, rules) = maximum quantities - minimum quantities
  where
    quantities = getOccurences $ runInsertion rules polymer 10

sol2 :: (Polymer, Rules) -> Int
sol2 (polymer, rules) = maximum ls - minimum ls
  where
    ls = getOccurences $ runInsertion rules polymer 40

getOccurences :: Polymer -> [Int]
getOccurences = map (\(a,b) -> b `quot` 2 + b `mod` 2) . MS.toOccurList . MS.concatMap (\x->x)

runInsertion :: Rules -> Polymer -> Int -> Polymer
runInsertion _ ms 0 = ms
runInsertion m ms i = runInsertion m (insertion ms m) (i-1)

insertion :: Polymer -> Rules-> Polymer
insertion ms m = MS.concatMap (\[a,b] -> let c = m Map.! [a,b] in [[a,c],[c,b]]) ms

input :: IO (Polymer, Rules)
input = parse <$> lines <$> readFile "Year2021/Inputs/Day14.txt"

example :: (Polymer, Rules)
example =  parse ["NNCB","","CH -> B","HH -> N","CB -> H","NH -> C","HB -> C","HC -> B","HN -> C","NN -> C","BH -> H","NC -> B","NB -> B","BN -> B","BB -> N","BC -> B","CC -> N","CN -> C"]

parse :: [String] -> (Polymer, Rules)
parse xs = (ms,m)
      where
        [str,instr] = split [""] xs
        ms = MS.fromList $ filter (\x -> length x == 2) $  map (take 2) $ tails $ head str
        m = Map.fromList $ map (\[x,y] -> (x, head y)) $ map (split " -> ") instr

d14sol1 :: IO Int
d14sol1 = sol1 <$> input
d14sol2 :: IO Int
d14sol2 = sol2 <$> input

prop_sol1 = 1588 == sol1 example
prop_sol2 = 2188189693529 == sol2 example

-- QuickCheck
return []
check = $quickCheckAll
