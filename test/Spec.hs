module Main (main) where

import qualified Spec.Parse
import qualified Spec.Eval

main :: IO ()
main = do
  Spec.Parse.runTests
  Spec.Eval.runTests
