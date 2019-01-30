module Main (main) where

import qualified Test.Tasty as T
import qualified Test.Tasty.Golden as G
import System.FilePath (replaceDirectory)
import System.FilePath.Glob (compile, globDir1)
import qualified Data.ByteString.Lazy.Char8 as C

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
        result      = (C.pack . show) <$> evalFile file

evalFile :: String -> IO String
evalFile file = undefined
