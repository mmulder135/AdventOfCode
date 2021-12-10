{-# LANGUAGE TemplateHaskell #-}
module Year2015.Solutions.Day0
  (
    d0sol1,
    d0sol2
  )
where
import Test.QuickCheck
import Test.QuickCheck.All

input :: IO String
input = readFile "Year2015/Inputs/Day01.txt"

-- QuickCheck
return []
check = $quickCheckAll
