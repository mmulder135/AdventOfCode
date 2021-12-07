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
  let yearDayArray = map (\y -> [year,(show y)]) [1..day]
  mapM_ runDayWithoutTest yearDayArray
runDay [year,day] = do
  putStrLn ("Solutions for day " ++ day ++ " of " ++ year)
  let funcs = [(test day "1"),(sol day "1"),(test day "2"),(sol day "2")]
  t1 <- Loader.runFile year funcs
  return t1
runDay [year,day,part] = do
  let funcs = [(test day part),(sol day part)]
  t1 <- Loader.runFile year funcs
  return t1
runDay _ = putStrLn usage

runDayWithoutTest :: [String] -> IO ()
runDayWithoutTest [year,day] = do
  putStrLn ("Solutions for day " ++ day)
  let funcs = [(sol day "1"),(sol day "2")]
  t1 <- Loader.runFile year funcs
  putStrLn ""

sol :: String -> String -> (String,Bool,String)
sol day part = (("d" ++ day ++ "sol" ++ part),True,("Solution for part " ++ part ++ ": "))

test :: String -> String -> (String,Bool,String)
test day part = (("d" ++ day ++ "test" ++ part),False,("Result of test " ++ part ++ ": "))
