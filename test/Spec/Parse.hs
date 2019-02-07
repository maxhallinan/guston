module Spec.Parse (runTests) where

import Test.Hspec (hspec, describe, it, shouldBe)
import Test.Hspec.Expectations (Expectation)
import Test.HUnit.Lang (assertFailure)
import Test.QuickCheck (Arbitrary, Gen, arbitrary, elements, property, resize)
import Test.QuickCheck.Gen (oneof, listOf)
import Test.QuickCheck.Instances.Char (lowerAlpha, nonSpace, numeric, upperAlpha)

import Syntax (Sexpr(..), SpecialForm(..))
import Parse (parseStr, parseFile)

runTests :: IO ()
runTests = hspec $ do
  describe "Parse" $ do
    describe "Parse.parseFile" $ do
      describe "special forms" $ do
        it "parses atom?" $ do
          "atom?" `parsesFileTo` [SFrm IsAtm]
        it "parses car" $ do
          "car" `parsesFileTo` [SFrm Car]
        it "parses cdr" $ do
          "cdr" `parsesFileTo` [SFrm Cdr]
        it "parses cns" $ do
          "cons" `parsesFileTo` [SFrm Cons]
        it "parses define" $ do
          "define" `parsesFileTo` [SFrm Def]
        it "parses eq?" $ do
          "eq?" `parsesFileTo` [SFrm IsEq]
        it "parses lambda" $ do
          "lambda" `parsesFileTo` [SFrm Lambda]
        it "parses quote" $ do
          "quote" `parsesFileTo` [SFrm Quote]

      describe "symbol" $ do
        it "parses a symbol" $ do
          property prop_parseFile_Sym

      describe "list" $ do
        it "parses a list" $ do
          property prop_parseFile_List

    describe "Parser.parseStr" $ do
      describe "special forms" $ do
        it "parses atom?" $ do
          "atom?" `parsesStrTo` (SFrm IsAtm)
        it "parses car" $ do
          "car" `parsesStrTo` (SFrm Car)
        it "parses cdr" $ do
          "cdr" `parsesStrTo` (SFrm Cdr)
        it "parses cons" $ do
          "cons" `parsesStrTo` (SFrm Cons)
        it "parses cond" $ do
          "cond" `parsesStrTo` (SFrm Cond)
        it "parses define" $ do
          "define" `parsesStrTo` (SFrm Def)
        it "parses eq?" $ do
          "eq?" `parsesStrTo` (SFrm IsEq)
        it "parses lambda" $ do
          "lambda" `parsesStrTo` (SFrm Lambda)
        it "parses quote" $ do
          "quote" `parsesStrTo` (SFrm Quote)

      describe "symbol" $ do
        it "parses a symbol" $ do
          property prop_parseStr_Sym

      describe "list" $ do
        it "parses a list" $ do
          property prop_parseStr_List

parsesFileTo :: String -> [Sexpr] -> Expectation
parsesFileTo file expr =
  case parseFile "" file of
    Right result  -> result `shouldBe` expr
    Left err      -> assertFailure $ show err

parsesStrTo :: String -> Sexpr -> Expectation
parsesStrTo str expr =
  case parseStr str of
    Right result  -> result `shouldBe` expr
    Left err      -> assertFailure $ show err

prop_parseFile_Sym :: ArbSym -> Bool
prop_parseFile_Sym (ArbSym s) = result == Right expected
  where result    = parseFile "" s
        expected  = [Sym s]

prop_parseFile_List :: ArbLst -> Bool
prop_parseFile_List (ArbLst l) =
  case parseFile "" l of
    Right [Lst _] ->
      True
    _ ->
      False

prop_parseStr_Sym :: ArbSym -> Bool
prop_parseStr_Sym (ArbSym s) = result == Right expected
  where result    = parseStr s
        expected  = Sym s

prop_parseStr_List :: ArbLst -> Bool
prop_parseStr_List (ArbLst l) =
  case parseStr l of
    Right (Lst _) ->
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
