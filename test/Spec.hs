module Main
  ( main
  ) where

import qualified Spec.Eval
import qualified Spec.Parse

main :: IO ()
main = do
  Spec.Parse.runTests
  Spec.Eval.runTests
