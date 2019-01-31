module Spec.Parse (run, sym) where

import Test.Hspec (hspec, describe, it)
import Test.QuickCheck (Arbitrary, Gen, arbitrary, elements, property)
import Test.QuickCheck.Gen (oneof, listOf)
import Test.QuickCheck.Instances.Char (lowerAlpha, upperAlpha, numeric)

import qualified Syntax as S
import Parse (parseString, parseFile)

run :: IO ()
run = hspec $ do
  describe "Parse" $ do
    describe "Parse.parseFile" $ do
      it "parses a symbol" $ do
        property prop_parseFile_Sym

    describe "Parser.parseString" $ do
      it "parses a symbol" $ do
        property prop_parseString_Sym

prop_parseFile_Sym :: Symbol -> Bool
prop_parseFile_Sym (Symbol s) = result == Right expected
  where result    = parseFile "" s
        expected  = S.List [S.Symbol s]

prop_parseString_Sym :: Symbol -> Bool
prop_parseString_Sym (Symbol s) = result == Right expected
  where result    = parseString s
        expected  = S.Symbol s

newtype Symbol = Symbol String deriving (Eq, Show)

instance Arbitrary Symbol where
  arbitrary = do
    s <- sym
    return $ Symbol s

alpha :: Gen Char
alpha = oneof [ lowerAlpha, upperAlpha ]

sym :: Gen String
sym = do
  i   <- initial
  sub <- listOf subsequent
  return (i : sub)
  where
    initial :: Gen Char
    initial = oneof [ elements "!$%&*/:<=>?~_^" 
                    , alpha
                    ]

    subsequent :: Gen Char
    subsequent =  oneof [ initial
                        , numeric
                        , elements ".+-"
                        ]
