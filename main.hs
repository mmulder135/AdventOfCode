import System.Environment (getArgs)
import Text.Read (readMaybe)
import qualified Solutions
import qualified Loader (getResult)

main :: IO ()
main = do
  args <- getArgs
  runDay args

usage :: String
usage = "Usage: main [day] ?[part]"

runDay :: [String] -> IO ()
runDay [day] = do
  putStrLn ("Solutions for day " ++ day)
  runTest day "1"
  runPart day "1"
  runTest day "2"
  runPart day "2"
runDay [day,part] = do
  putStrLn ("Solution for day " ++ day ++ " part " ++ part)
  runTest day part
  runPart day part
runDay _ = putStrLn usage

runPart :: String -> String -> IO ()
runPart day part  = do
  putStr ("Solution for part " ++ part ++ ": ")
  t1 <- Loader.getResult ("d" ++ day ++ "sol" ++ part) True
  return t1

runTest :: String -> String -> IO ()
runTest day test = do
  putStr ("Result of test " ++ test ++ ": ")
  t1 <- Loader.getResult ("d" ++ day ++ "test" ++ test) False
  return t1
