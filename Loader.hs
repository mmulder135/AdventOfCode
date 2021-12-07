module Loader
  (
  getResult
  ) where
import qualified Language.Haskell.Interpreter as Hint

parseFunc :: String -> Hint.Interpreter ()
parseFunc func = do
  Hint.loadModules ["Solutions.hs"]
  Hint.setImports ["Prelude","Solutions","System.IO.Unsafe"]
  a <- Hint.eval func
  Hint.liftIO $ print a

runFunc :: String -> IO ()
runFunc func  = do
  r <- Hint.runInterpreter $ parseFunc func
  case r of
    Left err -> putStrLn $ "Could not run function " ++ func
    Right x -> return x

getResult :: String -> Bool -> IO ()
getResult str io
        | io = runFunc $ "unsafePerformIO Solutions." ++ str
        | otherwise = runFunc $ "Solutions." ++ str
