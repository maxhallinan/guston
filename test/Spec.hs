module Main (main) where

import Test.Hspec (hspec, describe, it, shouldBe)

main :: IO ()
main = hspec $ describe "foo" $ do
  it "true is true" $
    True `shouldBe` True
