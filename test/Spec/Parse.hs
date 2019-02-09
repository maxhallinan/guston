module Spec.Parse (runTests) where

import Test.Hspec (hspec, describe, it, shouldBe)
import Test.Hspec.Expectations (Expectation)
import Test.HUnit.Lang (assertFailure)
import Test.QuickCheck (Arbitrary, Gen, arbitrary, elements, property, resize)
import Test.QuickCheck.Gen (oneof, listOf)
import Test.QuickCheck.Instances.Char (lowerAlpha, nonSpace, numeric, upperAlpha)

import Syntax (Expr(..), Info(..), Sexpr(..), SpecialForm(..))
import Parse (parseStr, parseFile)

runTests :: IO ()
runTests = hspec $ do
  describe "Parse" $ do
    describe "Parse.parseFile" $ do
      describe "special forms" $ do
        it "parses atom?" $ do
          "atom?" `parsesFileTo` [Expr (SFrm IsAtm) (Info 1 1 "")]
        it "parses car" $ do
          "car" `parsesFileTo` [Expr (SFrm Car) (Info 1 1 "")]
        it "parses cdr" $ do
          "cdr" `parsesFileTo` [Expr (SFrm Cdr) (Info 1 1 "")]
        it "parses cns" $ do
          "cons" `parsesFileTo` [Expr (SFrm Cons) (Info 1 1 "")]
        it "parses define" $ do
          "define" `parsesFileTo` [Expr (SFrm Def) (Info 1 1 "")]
        it "parses eq?" $ do
          "eq?" `parsesFileTo` [Expr (SFrm IsEq) (Info 1 1 "")]
        it "parses lambda" $ do
          "lambda" `parsesFileTo` [Expr (SFrm Lambda) (Info 1 1 "")]
        it "parses quote" $ do
          "quote" `parsesFileTo` [Expr (SFrm Quote) (Info 1 1 "")]

      describe "symbol" $ do
        it "parses a symbol" $ do
          property prop_parseFile_Sym

      describe "list" $ do
        it "parses a list" $ do
          property prop_parseFile_List

    describe "Parser.parseStr" $ do
      describe "special forms" $ do
        it "parses atom?" $ do
          "atom?" `parsesStrTo` Expr (SFrm IsAtm) (Info 1 1 "")
        it "parses car" $ do
          "car" `parsesStrTo` Expr (SFrm Car) (Info 1 1 "")
        it "parses cdr" $ do
          "cdr" `parsesStrTo` Expr (SFrm Cdr) (Info 1 1 "")
        it "parses cons" $ do
          "cons" `parsesStrTo` Expr (SFrm Cons) (Info 1 1 "")
        it "parses cond" $ do
          "cond" `parsesStrTo` Expr (SFrm Cond) (Info 1 1 "")
        it "parses define" $ do
          "define" `parsesStrTo` Expr (SFrm Def) (Info 1 1 "")
        it "parses eq?" $ do
          "eq?" `parsesStrTo` Expr (SFrm IsEq) (Info 1 1 "")
        it "parses lambda" $ do
          "lambda" `parsesStrTo` Expr (SFrm Lambda) (Info 1 1 "")
        it "parses quote" $ do
          "quote" `parsesStrTo` Expr (SFrm Quote) (Info 1 1 "")

      describe "symbol" $ do
        it "parses a symbol" $ do
          property prop_parseStr_Sym

      describe "list" $ do
        it "parses a list" $ do
          property prop_parseStr_List

parsesFileTo :: String -> [Expr] -> Expectation
parsesFileTo file expr =
  case parseFile "" file of
    Right result  -> result `shouldBe` expr
    Left err      -> assertFailure $ show err

parsesStrTo :: String -> Expr -> Expectation
parsesStrTo str expr =
  case parseStr str of
    Right result  -> result `shouldBe` expr
    Left err      -> assertFailure $ show err

prop_parseFile_Sym :: ArbSym -> Bool
prop_parseFile_Sym (ArbSym s) =
  case parseFile "" s of
    (Right [Expr result _]) ->
      result == Sym s
    _ ->
      False

prop_parseFile_List :: ArbLst -> Bool
prop_parseFile_List (ArbLst l) =
  case parseFile "" l of
    Right [Expr (Lst _) _] ->
      True
    _ ->
      False

prop_parseStr_Sym :: ArbSym -> Bool
prop_parseStr_Sym (ArbSym s) =
  case parseStr s of
    Right (Expr result _) ->
      result == Sym s
    _ ->
      False

prop_parseStr_List :: ArbLst -> Bool
prop_parseStr_List (ArbLst l) =
  case parseStr l of
    Right (Expr (Lst _) _) ->
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
  where sexprs = unwords <$> (resize 2 $ listOf sexpr')
