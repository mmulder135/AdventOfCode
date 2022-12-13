{-# LANGUAGE TemplateHaskell #-}
module Year2022.Solutions.Day07
  (
    d7sol1,
    d7sol2
  )
where
import Test.QuickCheck
import Test.QuickCheck.All
import Data.List.Utils      (startswith)
import Data.List            (sort)
import Data.Maybe           (Maybe, isJust, fromJust)

data FileTree = Dir String [FileTree] Int| File String Int 
  deriving (Show, Eq)

sol1 :: [String] -> Int
sol1 = sum . filter (<=100000) . getDirSizeList . calcDirSize . parseInput

sol2 :: [String] -> Int
sol2 input = head $ sort $ filter (>= needed) sizeList
  where
    free = 70000000 - head sizeList
    needed = 30000000 - free
    sizeList = getDirSizeList $ calcDirSize $ parseInput input

getDirSizeList :: FileTree -> [Int]
getDirSizeList (Dir _ tree size) = size : (concat $ map getDirSizeList tree)
getDirSizeList (File _ _ ) = []

calcDirSize :: FileTree -> FileTree
calcDirSize (Dir name tree _) = Dir name newTree size
    where
      newTree = map calcDirSize tree
      size = sum $ map getFTSize newTree
calcDirSize (File name size) = File name size

getFTSize :: FileTree -> Int
getFTSize (Dir _ _ size) = size
getFTSize (File _ size) = size

-- Parsing
parseInput :: [String] -> FileTree
parseInput str = fst $ parse str (Dir "root" [] 0)

parse :: [String] -> FileTree -> (FileTree, [String])
parse [] tree = (tree, [])
parse (c:rest) tree@(Dir name subt _) 
    | c == "$ cd .." = (tree, rest)
    | c == "$ ls" = parse unparsedls (Dir name newTreeList 0)
    | otherwise = parse unparsedsub (Dir name treelist 0) -- cd folder
      where
        (newTreeList, unparsedls) = parsels rest []
        (subparse, unparsedsub) = parse rest dir
        treelist = replaceDir subparse subt
        maybeDir = findDir subtreeName subt
        dir | isJust maybeDir = fromJust maybeDir
            | otherwise = Dir subtreeName [] 0
        subtreeName = last $ words c 

replaceDir :: FileTree -> [FileTree] -> [FileTree]
replaceDir new [] = [new]
replaceDir new@(Dir name subtree _) (old@(Dir n _ _):xs)
    | n == name = new : xs
    | otherwise = old : replaceDir new xs
replaceDir new (file@(File _ _):xs) = file : replaceDir new xs


findDir :: String -> [FileTree] -> Maybe FileTree
findDir str [] = Nothing
findDir str ((File _ _):xs) = findDir str xs
findDir str (dir@(Dir n _ _):xs)
    | n == str = Just dir
    | otherwise = findDir str xs

parsels :: [String] -> [FileTree]-> ([FileTree], [String])
parsels [] tree = (tree, [])
parsels (x:xs) tree
    | startswith "$" x = (tree, x:xs)
    | startswith "dir" x = parsels xs $ (Dir dirname [] 0) : tree
    | otherwise = parsels xs $ (File filename size) : tree
    where
      dirname = last $ words x
      [s, filename] = words x
      size::Int = read s

d7sol1 :: IO Int
d7sol1 = sol1 <$> input
d7sol2 :: IO Int
d7sol2 = sol2 <$> input

input :: IO [String]
input = lines <$> readFile "Year2022/Inputs/Day07.txt"

example :: [String]
example = [
  "$ cd /",
  "$ ls",
  "dir a",
  "14848514 b.txt",
  "8504156 c.dat",
  "dir d",
  "$ cd a",
  "$ ls",
  "dir e",
  "29116 f",
  "2557 g",
  "62596 h.lst",
  "$ cd e",
  "$ ls",
  "584 i",
  "$ cd ..",
  "$ cd ..",
  "$ cd d",
  "$ ls",
  "4060174 j",
  "8033020 d.log",
  "5626152 d.ext",
  "7214296 k"
  ]

prop_parse :: Bool
prop_parse = (parseInput example) == (
    Dir "root" [
      Dir "/" [
        Dir "d" [
          File "k" 7214296,
          File "d.ext" 5626152,
          File "d.log" 8033020,
          File "j" 4060174
          ] 0,
        File "c.dat" 8504156,
        File "b.txt" 14848514,
        Dir "a" [
          File "h.lst" 62596,
          File "g" 2557,
          File "f" 29116,
          Dir "e" [
            File "i" 584
            ] 0
          ] 0
        ] 0
      ] 0
    )
prop_calcdirsize1 = calcDirSize (Dir "e" [File "i" 584] 0) == (Dir "e" [File "i" 584] 584)
prop_calcdirsize2 = calcDirSize (Dir "a" [File "h.lst" 62596,File "g" 2557,File "f" 29116,Dir "e" [File "i" 584] 0] 0) == (Dir "a" [File "h.lst" 62596,File "g" 2557,File "f" 29116,Dir "e" [File "i" 584] 584] 94853)
prop_calcdirsize_example = (calcDirSize $ parseInput example) == (
    Dir "root" [
      Dir "/" [
        Dir "d" [
          File "k" 7214296,
          File "d.ext" 5626152,
          File "d.log" 8033020,
          File "j" 4060174
          ] 24933642,
        File "c.dat" 8504156,
        File "b.txt" 14848514,
        Dir "a" [
          File "h.lst" 62596,
          File "g" 2557,
          File "f" 29116,
          Dir "e" [
            File "i" 584
            ] 584
          ] 94853
        ] 48381165
      ] 48381165
    )
prop_sol1 = sol1 example == 95437
prop_sol2 = sol2 example == 24933642
-- QuickCheck
return []
check = $quickCheckAll
