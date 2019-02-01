module Spec.Eval (run) where

import Test.Hspec (hspec, describe, it, pending, shouldBe)
import Test.Hspec.Expectations (Expectation)
import Test.HUnit.Lang (assertFailure)

import qualified Syntax as S
import Eval (defaultEnv, evalInEnv)

run :: IO ()
run = hspec $ do
  describe "Eval" $ do
    describe "evalInEnv" $ do
      describe "special forms" $ do
        describe "atom?" $ do
          it "(atom? x) returns (quote t) when x is an atom" $ do
            evalShouldBe
              (S.Lst [S.Sym "atom?", S.Sym "x"])
              (S.Sym "t")
          it "(atom? x) returns (quote ()) when x is not an atom" $ do
            evalShouldBe
              (S.Lst [S.Sym "atom?", S.Lst []])
              (S.Lst [S.Sym "quote", S.Lst []])
        describe "car" $ do
          it "(car xs) returns the first item in xs" $ do
            evalShouldBe
              (S.Lst [S.Sym "car", S.Lst [S.Sym "x"]])
              (S.Sym "x")
          it "(car xs) throws an exception if xs is not a list" $ do
            pending
        describe "cdr" $ do
          it "(cdr xs) returns the tail of xs" $ do
            evalShouldBe
              (S.Lst [S.Sym "car", S.Lst [S.Sym "x", S.Sym "y", S.Sym "z"]])
              (S.Lst [S.Sym "y", S.Sym "z"])
          it "(cdr xs) returns an empty list if xs is empty" $ do
            evalShouldBe
              (S.Lst [S.Sym "cdr", S.Lst []])
              (S.Lst [])
          it "(car xs) throws an exception if xs is not a list" $ do
            pending
        describe "cond" $ do
          it "(cond cs) returns the value of the first true condition in cs" $ do
            pending
          it "(cond cs) returns false if cs is an empty list" $ do
            pending
          it "(cond cs) throws an exception if cs is not a list" $ do
            pending
        describe "cons" $ do
          it "(cons x y) adds item x to the head of list y" $ do
            pending
          it "(cons x y) returns a list of one if y is an empty list" $ do
            pending
          it "(cons x y) throws an exception if y is not a list" $ do
            pending
          it "(cons x) throws an exception" $ do
            pending
        describe "define" $ do
          it "(define foo y) binds the value of y to the symbol foo" $ do
            pending
          it "(define foo y) overrides the value bound to an existing symbol foo" $ do
            pending
          it "(define foo) throws an exception" $ do
            pending
        describe "eq?" $ do
          it "(eq x y) returns (quote t) if x and y are equivalent symbols" $ do
            pending
          it "(eq x y) returns (quote ()) if x and y are not equivalent symbols" $ do
            pending
          it "(eq x y) throws an exception if x and y are not symbols" $ do
            pending
        describe "lambda" $ do
          it "(lambda (x y) z) returns a function" $ do
            pending
          it "(lambda (x y)) throws an exception" $ do
            pending
          it "(lambda (x y)) throws an argument number exception" $ do
            pending
          it "(lambda x y) throws type error exception" $ do
            pending
        describe "quote" $ do
          it "(quote (x, y, z)) returns a quoted list" $ do
            pending
          it "(quote x) returns a quoted symbol" $ do
            pending
          it "(quote x y z) throws an argument exception" $ do
            pending
      describe "function application" $ do
          it "((lambda (x) x) (quote y)) evaluates to the symbol y" $ do
            pending
          it "((lambda (x y) y) (quote z)) throws an arguments exception" $ do
            pending
      describe "variable lookup" $ do
          it "evaluating x returns the value bound to the symbol x" $ do
            pending
          it "evaluating x throws an unknown variable exception when no value is bound to x" $ do
            pending

evalShouldBe :: S.Sexpr -> S.Sexpr -> Expectation
evalShouldBe expr expected = do
  (result, _) <- evalInEnv defaultEnv expr
  case result of
    (Right actual)  -> actual `shouldBe` expected
    (Left err)      -> assertFailure (show err)
