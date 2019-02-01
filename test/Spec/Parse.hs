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

prop_parseFile_Sym :: ArbSym -> Bool
prop_parseFile_Sym (ArbSym s) = result == Right expected
  where result    = parseFile "" s
        expected  = S.Lst [S.Sym s]

prop_parseFile_List :: ArbLst -> Bool
prop_parseFile_List (ArbLst l) =
  case parseFile "" l of
    Right (S.Lst [S.Lst _]) ->
      True
    _ ->
      False

prop_parseString_Sym :: ArbSym -> Bool
prop_parseString_Sym (ArbSym s) = result == Right expected
  where result    = parseString s
        expected  = S.Sym s

prop_parseString_List :: ArbLst -> Bool
prop_parseString_List (ArbLst l) =
  case parseString l of
    Right (S.Lst _) ->
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

newtype ArbSym = ArbSym String deriving (Eq, Show)

instance Arbitrary ArbSym where
  arbitrary = ArbSym <$> symbol

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

newtype ArbLst = ArbLst String deriving (Eq, Show)

instance Arbitrary ArbLst where
  arbitrary = ArbLst <$> list

list :: Gen String
list = parens sexprs
  where sexprs = unwords <$> (resize 2 $ listOf sexpr)
