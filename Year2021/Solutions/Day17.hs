{-# LANGUAGE TemplateHaskell #-}
module Year2021.Solutions.Day17
  (
    -- d0sol1,
    -- d0sol2
  )
where
import Test.QuickCheck
import Test.QuickCheck.All
import Data.Maybe (isJust)

type Velocity = (Int,Int)
type Coord = (Int,Int)
type Area = (Coord,Coord)
type Probe = (Velocity,Coord)
a = ((20,30),(-10,-5)) :: Area
input = ((143,177),(-106,-71)) :: Area

sol1 :: Area -> Int
sol1 = foldr getMax 0 . bruteforce
    where
      getMax :: Maybe Int -> Int -> Int
      getMax (Just y) x = max x y
      getMax Nothing x = x

sol2 :: Area -> Int
sol2 = length . bruteforce

bruteforce :: Area -> [Maybe Int]
bruteforce area@((_,xmax),(ymax,_)) = filter (isJust) $ map (shoot area) $ concatMap (\y -> zip [0..xmax] $ repeat y) [ymax..(-ymax)]

shoot :: Area -> Velocity -> Maybe Int
shoot area v = shoot' area (v,(0,0)) 0

shoot' :: Area -> Probe -> Int -> Maybe Int
shoot' area@((xmin,xmax),(ymax,ymin)) probe@(_,(x,y)) top
        | x > xmax || y < ymax = Nothing
        | x >= xmin && y <= ymin = Just top
        | otherwise = shoot' area (step probe) nt
        where
          nt = if y > top then y else top

step :: Probe -> Probe
step ((xv,yv),(x,y)) =  ((xv - dx, yv - 1), (x + xv, y + yv))
      where
        dx = if xv == 0 then 0 else signum (x + xv)

-- d0sol1 :: IO Int
-- d0sol1 = sol1 <$> input
-- d0sol2 :: IO Int
-- d0sol2 = sol2 <$> input
prop_sol1 = 45 == sol1 a
prop_sol1i = 5565 == sol1 input
prop_sol2 = 112 == sol2 a
prop_sol2i = 2118 == sol2 input
-- QuickCheck
return []
check = $quickCheckAll
