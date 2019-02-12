module Spec.Parse (runTests) where

import Test.Hspec (describe, hspec, it, shouldBe)
import Test.Hspec.Expectations (Expectation)
import Test.HUnit.Lang (assertFailure)
import Test.QuickCheck (Arbitrary, Gen, arbitrary, elements, property, resize, suchThat)
import Test.QuickCheck.Gen (listOf, oneof)
import Test.QuickCheck.Instances.Char (lowerAlpha, nonSpace, numeric, upperAlpha)

import Parse (parseFile, parseStr)
import Syntax (Expr (..), Info (..), SpecialForm (..), XExpr (..))

runTests :: IO ()
runTests = hspec $ do
  describe "Parse" $ do
    describe "Parse.parseFile" $ do
      describe "special forms" $ do
        it "parses atom?" $ do
          "atom?" `parsesFileTo` [XExpr (SFrm IsAtm) (Info (1,1))]
        it "parses first" $ do
          "first" `parsesFileTo` [XExpr (SFrm First) (Info (1,1))]
        it "parses rest" $ do
          "rest" `parsesFileTo` [XExpr (SFrm Rest) (Info (1,1))]
        it "parses ::" $ do
          "::" `parsesFileTo` [XExpr (SFrm Cons) (Info (1,1))]
        it "parses =" $ do
          "=" `parsesFileTo` [XExpr (SFrm Def) (Info (1,1))]
        it "parses ==" $ do
          "==" `parsesFileTo` [XExpr (SFrm IsEq) (Info (1,1))]
        it "parses fn" $ do
          "fn" `parsesFileTo` [XExpr (SFrm Lambda) (Info (1,1))]
        it "parses quote" $ do
          "quote" `parsesFileTo` [XExpr (SFrm Quote) (Info (1,1))]
        it "parses if" $ do
          "if" `parsesFileTo` [XExpr (SFrm If) (Info (1,1))]

      describe "symbol" $ do
        it "parses a symbol" $ do
          property prop_parseFile_Sym

      describe "list" $ do
        it "parses a list" $ do
          property prop_parseFile_List

    describe "Parser.parseStr" $ do
      describe "special forms" $ do
        it "parses atom?" $ do
          "atom?" `parsesStrTo` XExpr (SFrm IsAtm) (Info (1,1))
        it "parses first" $ do
          "first" `parsesStrTo` XExpr (SFrm First) (Info (1,1))
        it "parses rest" $ do
          "rest" `parsesStrTo` XExpr (SFrm Rest) (Info (1,1))
        it "parses ::" $ do
          "::" `parsesStrTo` XExpr (SFrm Cons) (Info (1,1))
        it "parses if" $ do
          "if" `parsesStrTo` XExpr (SFrm If) (Info (1,1))
        it "parses =" $ do
          "=" `parsesStrTo` XExpr (SFrm Def) (Info (1,1))
        it "parses ==" $ do
          "==" `parsesStrTo` XExpr (SFrm IsEq) (Info (1,1))
        it "parses fn" $ do
          "fn" `parsesStrTo` XExpr (SFrm Lambda) (Info (1,1))
        it "parses quote" $ do
          "quote" `parsesStrTo` XExpr (SFrm Quote) (Info (1,1))

      describe "symbol" $ do
        it "parses a symbol" $ do
          property prop_parseStr_Sym

      describe "list" $ do
        it "parses a list" $ do
          property prop_parseStr_List

parsesFileTo :: String -> [XExpr] -> Expectation
parsesFileTo file expr =
  case parseFile "" file of
    Right result -> result `shouldBe` expr
    Left err     -> assertFailure $ show err

parsesStrTo :: String -> XExpr -> Expectation
parsesStrTo str expr =
  case parseStr str of
    Right result -> result `shouldBe` expr
    Left err     -> assertFailure $ show err

prop_parseFile_Sym :: ArbSym -> Bool
prop_parseFile_Sym (ArbSym s) =
  case parseFile "" s of
    (Right [XExpr result _]) ->
      result == Sym s
    _ ->
      False

prop_parseFile_List :: ArbLst -> Bool
prop_parseFile_List (ArbLst l) =
  case parseFile "" l of
    Right [XExpr (Lst _) _] ->
      True
    _ ->
      False

prop_parseStr_Sym :: ArbSym -> Bool
prop_parseStr_Sym (ArbSym s) =
  case parseStr s of
    Right (XExpr result _) ->
      result == Sym s
    _ ->
      False

prop_parseStr_List :: ArbLst -> Bool
prop_parseStr_List (ArbLst l) =
  case parseStr l of
    Right (XExpr (Lst _) _) ->
      True
    _ ->
      False

sexpr' :: Gen String
sexpr' = oneof [ atom, list ]

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

isNotSFrm :: String -> Bool
isNotSFrm x = not $ elem x sFrms
  where sFrms = ["::", "=", "==", "atom?", "first", "fn", "if", "quote", "rest"]

symbol :: Gen String
symbol = flip suchThat isNotSFrm $ do
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
  where sexprs = unwords <$> (resize 2 $ listOf sexpr')
