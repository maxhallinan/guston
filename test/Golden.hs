module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Test.Tasty as T
import qualified Test.Tasty.Golden as G
import System.FilePath (replaceDirectory, addExtension)
import System.FilePath.Glob (compile, globDir1)

import qualified Eval as E
import Parse (parseFile)
import Syntax (defaultEnv)

main :: IO ()
main = do
  paths <- globDir1 (compile "*.gus") "examples"
  files <- traverse readSourceFile paths
  T.defaultMain $ T.testGroup "golden tests" $ runTest <$> files

readSourceFile :: String -> IO (String, String)
readSourceFile filepath = do
  file <- readFile filepath
  return (filepath, file)

runTest :: (String, String) -> T.TestTree
runTest (filepath, file) = G.goldenVsString filepath answerFile result
  where answerFile  = toGoldenPath filepath
        result      = (C.pack . show) <$> evalFile filepath file
        toGoldenPath = (flip addExtension "golden" . flip replaceDirectory "golden-files/")

evalFile :: String -> String -> IO String
evalFile filepath file = do
  case parseFile filepath file of
    Right sexpr -> do
      (result, _) <- E.runFile defaultEnv sexpr
      case result of
        Right []      -> return ""
        Right rs      -> return $ (show . head . reverse) rs
        Left evalErr  -> return $ show evalErr
    Left err -> return $ show err
