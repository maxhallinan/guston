module Main (main) where

import Control.Monad.State (runStateT)
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Test.Tasty as T
import qualified Test.Tasty.Golden as G
import System.FilePath (replaceDirectory)
import System.FilePath.Glob (compile, globDir1)

import qualified Eval as E
import Parse (parseFile)
import Syntax (defaultEnv)

main :: IO ()
main = do
  paths <- globDir1 (compile "*.wiz") "examples"
  putStrLn $ show (replaceDirectory "golden-files" <$> paths)
  files <- traverse readSourceFile paths
  T.defaultMain $ T.testGroup "golden tests" $ runTest <$> files

readSourceFile :: String -> IO (String, String)
readSourceFile filepath = do
  file <- readFile filepath
  return (filepath, file)

runTest :: (String, String) -> T.TestTree
runTest (filepath, file) = G.goldenVsString filepath answerFile result
  where answerFile  = replaceDirectory filepath "golden-files/"
        result      = (C.pack . show) <$> evalFile filepath file

evalFile :: String -> String -> IO String
evalFile filepath file = do
  case parseFile filepath file of
    Right ast -> do
      (result, _) <- (runStateT . E.runEval) (E.evalFile ast) defaultEnv
      case result of
        []  -> return ""
        _   -> return $ ((either show show) . head . reverse) result
    Left err -> return $ show err
