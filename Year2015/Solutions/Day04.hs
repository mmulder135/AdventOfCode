{-# LANGUAGE TemplateHaskell #-}
module Year2015.Solutions.Day04
  (
    d4sol1,
    d4sol2
  )
where
import Test.QuickCheck
import Test.QuickCheck.All
import Data.Hash.MD5
import Data.String.Utils

input = "bgvyzdsv" :: String

sol1 = solve 0 "00000"
sol2 = solve 0 "000000"

solve :: Int -> String -> String -> Int
solve i find str
    | take 5 hash == find = i
    | otherwise = solve (i+1) find str
      where
        curStr = str ++ (show i)
        hash = md5s (Str curStr)

prop_1_1 = sol1 "abcdef" == 609043
prop_1_2 = sol1 "pqrstuv" == 1048970

d4sol1 :: Int
d4sol1 = sol1 input
d4sol2 = sol2 input
-- QuickCheck
return []
check = $quickCheckAll
