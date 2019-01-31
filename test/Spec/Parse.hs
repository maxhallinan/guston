module Spec.Parse where

import Test.Hspec (hspec, describe, it)
import Test.QuickCheck (Arbitrary, Gen, arbitrary, elements, property, resize)
import Test.QuickCheck.Gen (oneof, listOf)
import Test.QuickCheck.Instances.Char (lowerAlpha, nonSpace, numeric, upperAlpha)

import qualified Syntax as S
import Parse (parseString, parseFile)

run :: IO ()
run = hspec $ do
  describe "Parse" $ do
    describe "Parse.parseFile" $ do
      it "parses a symbol" $ do
        property prop_parseFile_Sym
      it "parses a list" $ do
        property prop_parseFile_List

    describe "Parser.parseString" $ do
      it "parses a symbol" $ do
        property prop_parseString_Sym
      it "parses a list" $ do
        property prop_parseString_List

prop_parseFile_Sym :: ArbSymbol -> Bool
prop_parseFile_Sym (ArbSymbol s) = result == Right expected
  where result    = parseFile "" s
        expected  = S.List [S.Symbol s]

prop_parseFile_List :: ArbList -> Bool
prop_parseFile_List (ArbList l) =
  case parseFile "" l of
    Right (S.List [S.List _]) ->
      True
    _ ->
      False

prop_parseString_Sym :: ArbSymbol -> Bool
prop_parseString_Sym (ArbSymbol s) = result == Right expected
  where result    = parseString s
        expected  = S.Symbol s

prop_parseString_List :: ArbList -> Bool
prop_parseString_List (ArbList l) =
  case parseString l of
    Right (S.List _) ->
      True
    _ ->
      False

sexpr :: Gen String
sexpr = oneof [ atom, list ]

lineComment :: Gen String
lineComment = do
  s <- listOf nonSpace
  return $ unwords [";", s, "\n"]

atom :: Gen String
atom = token $ oneof [ symbol ]

token :: Gen String -> Gen String
token gen = do
  s1  <- space
  x   <- gen
  s2  <- space
  return (s1 ++ x ++ s2)
  where space       = oneof [ whitespace
                            , lineComment
                            ]
        whitespace  = listOf $ elements " \n"

parens :: Gen String -> Gen String
parens gen = do
  p1  <- token $ return "("
  x   <- gen
  p2  <- token $ return ")"
  return (p1 ++ x ++ p2)

newtype ArbSymbol = ArbSymbol String deriving (Eq, Show)

instance Arbitrary ArbSymbol where
  arbitrary = ArbSymbol <$> symbol

alpha :: Gen Char
alpha = oneof [ lowerAlpha, upperAlpha ]

symbol :: Gen String
symbol = do
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

newtype ArbList = ArbList String deriving (Eq, Show)

instance Arbitrary ArbList where
  arbitrary = ArbList <$> list

list :: Gen String
list = parens sexprs
  where sexprs = unwords <$> (resize 2 $ listOf sexpr)
