module Solutions.Day03
( d3sol1,
  d3sol2,
  d3test1,
  d3test2
)
where
import Data.Char
import Data.List

sol1 :: [String] -> Int
sol1 = power_cons

sol2 :: [String] -> Int
sol2 = life_sup_rating

example :: [String]
example = ["00100","11110","10110","10111","10101","01111","00111","11100","10000","11001","00010","01010"]

input :: IO [String]
input = words <$> readFile "Inputs/Day03.txt"

-- sol1
power_cons :: [String] -> Int
power_cons x = eps * gamma
      where
        eps = bin_to_dec (eps_bin x)
        gamma = bin_to_dec (gamma_bin x)

-- sol2
life_sup_rating :: [String] -> Int
life_sup_rating x = ox_rate * co2_rate
      where
        ox_rate = bin_to_dec $ find_rating ox_gen x
        co2_rate = bin_to_dec $ find_rating co2_scrub x

eps_bin :: [String] -> String
eps_bin x = count_to_bin zeros ones
      where
        ones = map count_one y
        zeros = map count_zero y
        y = transpose x

gamma_bin :: [String] -> String
gamma_bin x = count_to_bin ones zeros
      where
        ones = map count_one y
        zeros = map count_zero y
        y = transpose x


find_rating :: (Int -> Int -> Char) -> [String] -> String
find_rating func [] = ""
find_rating func [x] = x
find_rating func x = y : find_rating func rest
  where
    num_ones = count_one $ head $ transpose x
    num_zeros = count_zero $ head $ transpose x
    y = func num_ones num_zeros
    rest = map tail $ filter (\z -> head z ==  y) x


count_zero :: String -> Int
count_zero = length. filter (=='0')

count_one :: String -> Int
count_one = length . filter (=='1')

-- count_to_bin one_count zero_count
count_to_bin :: [Int] -> [Int] -> String
count_to_bin [] [] = ""
count_to_bin (x:xs) (y:ys)
        | x > y = '1' : count_to_bin xs ys
        | otherwise = '0' : count_to_bin xs ys

bin_to_dec :: String -> Int
bin_to_dec x = bin_to_dec' $ reverse x

bin_to_dec' :: String -> Int
bin_to_dec' [] = 0
bin_to_dec' (x:xs) = (digitToInt x) + 2 * bin_to_dec' xs

-- ones zeros
ox_gen :: Int -> Int -> Char
ox_gen x y
      | x >= y = '1'
      | otherwise = '0'

co2_scrub :: Int -> Int -> Char
co2_scrub x y
      | x >= y = '0'
      | otherwise = '1'

e1, e2 :: Int
e1 = 198
e2 = 230

d3sol1 :: IO Int
d3sol1 = sol1 <$> input
d3sol2 :: IO Int
d3sol2 = sol2 <$> input
d3test1 :: Bool
d3test1 = sol1 example == e1
d3test2 :: Bool
d3test2 = sol2 example == e2
