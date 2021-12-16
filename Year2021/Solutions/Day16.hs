{-# LANGUAGE TemplateHaskell #-}
module Year2021.Solutions.Day16
  (
    d16sol1,
    d16sol2
  )
where
import Test.QuickCheck
import Test.QuickCheck.All
import Data.Char (digitToInt)
import Data.List (foldl')

type Version = Int
type Type = Int
data Packet = Literal Version Int | Operator Version Type [Packet]
    deriving Show

sol1 :: String -> Int
sol1 = sumVersions . fst . parsePacket . hexToBin
sol2 :: String -> Int
sol2 = evaluate . fst . parsePacket . hexToBin

evaluate :: Packet -> Int
evaluate (Literal _ i) = i
evaluate (Operator _ t subs)
      | t == 0 = sum children
      | t == 1 = foldr (*) 1 children
      | t == 2 = minimum children
      | t == 3 = maximum children
      | t == 5 = comp (>) children
      | t == 6 = comp (<) children
      | t == 7 = comp (==) children
        where
          children = map evaluate subs :: [Int]
          comp :: (a -> a -> Bool) -> [a] -> Int
          comp f [h,t] = fromEnum $ f h t

sumVersions :: Packet -> Int
sumVersions (Literal v _) = v
sumVersions (Operator v _ ps) = v + sum (map sumVersions ps)

parsePacket :: String -> (Packet,String)
parsePacket bin
  | typ == 4 = (Literal v $ binToDec val, litleft)
  | ltyp == '0' = (Operator v typ sub0, left0)
  | otherwise = (Operator v typ sub1, left1)
  where
    (val, litleft) = getLiteralValue "" body
    (v,rest) = splitVal bin
    (typ, body) = splitVal rest
    (ltyp:subs) = body
    (sub0, left0) = parseSubPackets0 subs
    (sub1, left1) = parseSubPackets1 subs

getLiteralValue :: String -> String -> (String, String)
getLiteralValue ps xs
  | h == '1' =  getLiteralValue (ps ++ part) rest
  | otherwise = ((ps ++ part), rest)
    where
      ((h:part), rest) = splitAt 5 xs

parseSubPackets0 :: String -> ([Packet],String)
parseSubPackets0 xs = (parsePackets subs, left)
    where
      (l,body) = splitAt 15 xs
      len = binToDec l :: Int
      (subs, left) = splitAt len body

parseSubPackets1 :: String -> ([Packet], String)
parseSubPackets1 xs = parseNumPackets num body []
    where
      (n, body) = splitAt 11 xs
      num = binToDec n :: Int

parseNumPackets :: Int -> String -> [Packet] -> ([Packet], String)
parseNumPackets 0 str ps = (ps,str)
parseNumPackets i str ps = parseNumPackets (i-1) rest (ps ++ [p])
    where
      (p,rest) = parsePacket str

parsePackets :: String -> [Packet]
parsePackets [] = []
parsePackets xs = p : parsePackets rest
    where
      (p, rest) = parsePacket xs

splitVal :: String -> (Int, String)
splitVal xs = (binToDec h, rest)
    where
      (h,rest) = splitAt 3 xs

hexToBin :: String -> String
hexToBin = concatMap hexToBin'

hexToBin' :: Char -> [Char]
hexToBin' '0' = "0000"
hexToBin' '1' = "0001"
hexToBin' '2' = "0010"
hexToBin' '3' = "0011"
hexToBin' '4' = "0100"
hexToBin' '5' = "0101"
hexToBin' '6' = "0110"
hexToBin' '7' = "0111"
hexToBin' '8' = "1000"
hexToBin' '9' = "1001"
hexToBin' 'A' = "1010"
hexToBin' 'B' = "1011"
hexToBin' 'C' = "1100"
hexToBin' 'D' = "1101"
hexToBin' 'E' = "1110"
hexToBin' 'F' = "1111"

binToDec :: String -> Int
binToDec  = foldl' (\b a -> (digitToInt a) + 2 * b) 0

d16sol1 :: IO Int
d16sol1 = sol1 <$> input
d16sol2 :: IO Int
d16sol2 = sol2 <$> input

input :: IO String
input = readFile "Year2021/Inputs/Day16.txt"

prop_hextodex = hexToBin "D2FE28" == "110100101111111000101000"
prop_version = 6 == fst (splitVal $ hexToBin "D2FE28")
prop_getvalue = "011111100101" == fst (getLiteralValue "" $ drop 6 $ hexToBin "D2FE28")
prop_11 = 16 == sol1 "8A004A801A8002F478"
prop_12 = 12 == sol1 "620080001611562C8802118E34"
prop_13 = 23 == sol1 "C0015000016115A2E0802F182340"
prop_14 = 31 == sol1 "A0016C880162017C3686B18A3D4780"
prop_2Sum = 3 == sol2 "C200B40A82"
prop_2Prod = 54 == sol2 "04005AC33890"
prop_2Min = 7 == sol2 "880086C3E88112"
prop_2Max = 9 == sol2 "CE00C43D881120"
prop_2Less = 1 == sol2 "D8005AC2A8F0"
prop_2Gr = 0 == sol2 "F600BC2D8F"
prop_2Eq = 0 == sol2 "9C005AC2F8F0"
prop_2Comb = 1 == sol2 "9C0141080250320F1802104A08"
-- QuickCheck
return []
check = $quickCheckAll
