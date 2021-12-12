{-# LANGUAGE TemplateHaskell #-}
module Year2021.Solutions.Day0
  (
    -- d0sol1,
    -- d0sol2
  )
where
import Test.QuickCheck
import Test.QuickCheck.All



-- d0sol1 :: IO Int
-- d0sol1 = sol1 <$> input
-- d0sol2 :: IO Int
-- d0sol2 = sol2 <$> input

input :: IO String
input = readFile "Year2021/Inputs/Day0.txt"

-- QuickCheck
return []
check = $quickCheckAll
