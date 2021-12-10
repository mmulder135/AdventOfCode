import System.Environment (getArgs)
import System.Directory (listDirectory)
import Text.Read (readMaybe)
import Data.List.Utils (split)
import Data.List (sort)
import qualified Loader (runFile)

main :: IO ()
main = do
  args <- getArgs
  runDay args

usage :: String
usage = "Usage: main [year] [day] ?[part]"

runDay :: [String] -> IO ()
runDay [year] = do
  putStrLn ("All solutions of " ++ year)
  solutions <- listDirectory ("Year"++year++"/Solutions")
  let day = (read::String->Int) $ last  $  split "y"  $  head  $  split "."  $  last $ sort $ solutions
  let funcs = concatMap (\d -> [(sol (show d) "1"),(sol (show d) "2"),("","newline")]) [1..day]
  t1 <- Loader.runFile year funcs
  return t1
runDay [year,day] = do
  putStrLn ("Solutions for day " ++ day ++ " of " ++ year)
  let funcs = [(sol day "1"),(sol day "2")]
  t1 <- Loader.runFile year funcs
  return t1
runDay [year,day,part] = do
  let funcs = [(sol day part)]
  t1 <- Loader.runFile year funcs
  return t1
runDay _ = putStrLn usage

sol :: String -> String -> (String,String)
sol day part = (("d" ++ day ++ "sol" ++ part),("Solution for day " ++ day ++ " part " ++ part ++ ": "))
