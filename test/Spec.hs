module Main (main) where

import Spec.Parse

main :: IO ()
main = do
  Spec.Parse.run
