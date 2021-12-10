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
    parse i@(func,name)
          | name == "newline" = say ""
          | otherwise = parse' i
    parse' (func, name) = do
        a <- Hint.eval $ "unsafePerformIO Year" ++ year ++ ".Solutions." ++ func
        say $ name ++ show a

runFile :: String -> [(String,String)] -> IO ()
runFile year funcs  = do
  r <- Hint.runInterpreter $ parseFunc year funcs
  case r of
    Left err -> putStrLn $ "Could not run functions"
    Right x -> return x
