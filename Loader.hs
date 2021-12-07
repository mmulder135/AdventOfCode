module Loader
  (
  runFile
  ) where
import qualified Language.Haskell.Interpreter as Hint
say :: String -> Hint.Interpreter ()
say = Hint.liftIO . putStrLn

parseFunc :: String -> [(String,String)] -> Hint.Interpreter ()
parseFunc year funcs = do
  Hint.loadModules ["Year" ++ year ++ ".Solutions"]
  Hint.setImports ["Prelude","Year" ++ year ++ ".Solutions","System.IO.Unsafe"]
  mapM_ parse funcs
  where
    parse (func, name) = do
        a <- Hint.eval func
        say $ name ++ show a

runFunc :: String -> [(String,String)] -> IO ()
runFunc year funcs  = do
  r <- Hint.runInterpreter $ parseFunc year funcs
  case r of
    Left err -> putStrLn $ "Could not run functions"
    Right x -> return x

getFuncName :: String -> String -> Bool -> String
getFuncName year func io
        | io = "unsafePerformIO Year" ++ year ++ ".Solutions." ++ func
        | otherwise = "Year" ++ year ++ ".Solutions." ++ func

runFile :: String -> [(String,Bool,String)] -> IO ()
runFile year funcs = runFunc year $ map (\(func,io,str) -> ((getFuncName year func io),str)) funcs
