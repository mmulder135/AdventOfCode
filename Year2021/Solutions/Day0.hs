{-# LANGUAGE TemplateHaskell #-}
module Year2021.Solutions.Day0
  (
    d0sol1,
    d0sol2
  )
where
import Test.QuickCheck
import Test.QuickCheck.All

input :: IO String
input = readFile "Year2021/Inputs/Day0.txt"

-- QuickCheck
return []
check = $quickCheckAll
