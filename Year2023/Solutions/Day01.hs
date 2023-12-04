module Year2023.Solutions.Day01 ( d1sol1, d1sol2 ) where
import Data.Char ( isDigit )
import Data.List.Utils (replace)

d1sol1, d1sol2 :: IO Int
d1sol1 = sol1 <$> input
d1sol2 = sol2 <$> input

example :: [String]
example = ["1abc2","pqr3stu8vwx","a1b2c3d4e5f","treb7uchet"]

example2 :: [String]
example2 = ["two1nine","eightwothree","abcone2threexyz","xtwone3four","4nineeightseven2","zoneight234","7pqrstsixteen"]

input :: IO [String]
input = lines <$> readFile "src2022/Input/Day01.txt"

sol1 :: [String] -> Int
sol1 = sum . map getValue

getFirstLast :: String -> String
getFirstLast x = [head x, last x]

getValue :: String -> Int
getValue  = read . getFirstLast . filter isDigit

sol2 :: [String] -> Int
sol2 = sol1 . map translate

translate :: String -> String
translate = replace "one" "one1one" .
            replace "two" "two2two" .
            replace "three" "three3three" .
            replace "four" "four4four" .
            replace "five" "five5five" .
            replace "six" "six6six" .
            replace "seven" "seven7seven" .
            replace "eight" "eight8eight" .
            replace "nine" "nine9nine"
