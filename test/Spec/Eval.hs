module Spec.Eval (run) where

import Control.Monad.State (runStateT)
import qualified Data.Map as M
import Test.Hspec (hspec, describe, it, pending, shouldBe)
import Test.Hspec.Expectations (Expectation)
import Test.HUnit.Lang (assertFailure)

import qualified Syntax as S
import Eval (defaultEnv, eval, runEval)

run :: IO ()
run = hspec $ do
  describe "Eval" $ do
    describe "Eval.eval" $ do

      describe "special forms" $ do
        describe "atom?" $ do
          it "(atom? x) evaluates to true when x is an atom" $ do
            (S.Lst [S.Sym "atom?", S.Sym "x"]) `evaluatesTo` (S.Sym "true")
          it "(atom? x) evaluates to false when x is not an atom" $ do
            (S.Lst [S.Sym "atom?", S.Lst []])
            `evaluatesTo`
            (S.Sym "false")

        describe "car" $ do
          it "(car xs) returns the first item in xs" $ do
            (S.Lst [S.Sym "car", S.Lst [S.Sym "x"]]) `evaluatesTo` (S.Sym "x")
          it "(car xs) throws an exception if xs is not a list" $ do
            pending

        describe "cdr" $ do
          it "(cdr xs) returns the tail of xs" $ do
            (S.Lst [S.Sym "cdr", S.Lst [S.Sym "x", S.Sym "y", S.Sym "z"]])
            `evaluatesTo`
            (S.Lst [S.Sym "y", S.Sym "z"])
          it "(cdr xs) returns an empty list if xs is empty" $ do
            (S.Lst [S.Sym "cdr", S.Lst []])
            `evaluatesTo`
            (S.Lst [])
          it "(car xs) throws an exception if xs is not a list" $ do
            pending

        describe "cond" $ do
          it "(cond cs) returns the value of the first true condition in cs" $ do
            (S.Lst [ S.Sym "cond"
                   , S.Lst [ S.Lst [ S.Lst [ S.Sym "quote", S.Sym "false" ]
                                   , S.Lst [ S.Sym "quote", S.Sym "x" ]]
                           , S.Lst [ S.Lst [ S.Sym "quote", S.Sym "true" ]
                                   , S.Lst [ S.Sym "quote", S.Sym "y" ]]]])
            `evaluatesTo`
            (S.Sym "y")
          it "(cond cs) throws an exception if cs is an empty list" $ do
            pending
          it "(cond cs) throws an exception if cs is not a list" $ do
            pending

        describe "cons" $ do
          it "(cons x y) adds item x to the head of list y" $ do
            (S.Lst [ S.Sym "cons"
                   , S.Lst [ S.Sym "quote", S.Sym "x" ]
                   , S.Lst [ S.Sym "quote", S.Lst [S.Sym "y", S.Sym "z"]]])
            `evaluatesTo`
            (S.Lst [S.Sym "x", S.Sym "y", S.Sym "z"])
          it "(cons x y) returns a list of one if y is an empty list" $ do
            (S.Lst [ S.Sym "cons"
                   , S.Lst [ S.Sym "quote", S.Sym "x" ]
                   , S.Lst [ S.Sym "quote", S.Lst []]])
            `evaluatesTo`
            (S.Lst [S.Sym "x"])
          it "(cons x y) throws an exception if y is not a list" $ do
            pending
          it "(cons x) throws an exception" $ do
            pending

        describe "define" $ do
          it "(define foo y) binds the value of y to the symbol foo" $ do
            (S.Lst [ S.Sym "define"
                   , S.Sym "foo"
                   , S.Lst [ S.Sym "quote", S.Sym "x" ]
                   , S.Lst [ S.Sym "foo" ]])
            `insertsInEnv`
            ("foo", S.Sym "x")
          it "(define foo y) overrides the value bound to an existing symbol foo" $ do
            (S.Lst [ S.Sym "define"
                   , S.Sym "foo"
                   , S.Lst [ S.Sym "quote", S.Sym "x" ]
                   , S.Lst [ S.Sym "define"
                           , S.Sym "foo"
                           , S.Lst [ S.Sym "quote", S.Sym "y" ]
                           , S.Lst [ S.Sym "foo" ]
                           ]])
            `insertsInEnv`
            ("foo", S.Sym "y")
          it "(define foo) throws an exception" $ do
            pending

        describe "eq?" $ do
          it "(eq x y) returns true if x and y are equivalent symbols" $ do
            (S.Lst [S.Sym "eq?", S.Sym "x", S.Sym "x"])
            `evaluatesTo`
            (S.Sym "true")
          it "(eq x y) returns false if x and y are not equivalent symbols" $ do
            (S.Lst [S.Sym "eq?", S.Sym "x", S.Sym "y"])
            `evaluatesTo`
            (S.Sym "false")
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
          it "(quote (x, y)) evaluates to (x y)" $ do
            (S.Lst [S.Sym "quote", S.Lst [S.Sym "x", S.Sym "y"]])
            `evaluatesTo`
            (S.Lst [S.Sym "x", S.Sym "y"])
          it "(quote x) evaluates to the symbol x" $ do
            (S.Lst [S.Sym "quote", S.Sym "x"]) `evaluatesTo` (S.Sym "x")
          it "(quote x y z) throws an argument exception" $ do
            pending

      describe "function application" $ do
          it "((lambda (x) x) (quote y)) evaluates to the symbol y" $ do
            (S.Lst [ S.Lst [ S.Sym "lambda", S.Lst [ S.Sym "x" ]
                           , S.Sym "x" ]
                   , S.Lst [ S.Sym "quote", S.Sym "y" ]])
            `evaluatesTo`
            (S.Sym "y")
          it "((lambda (x y) y) (quote z)) throws an arguments exception" $ do
            pending

      describe "variable lookup" $ do
          it "evaluating x returns the value bound to the symbol x" $ do
            (S.Lst [ S.Sym "define"
                   , S.Lst [ S.Sym "quote", S.Sym "foo" ]
                   , S.Lst [ S.Sym "quote", S.Sym "x" ]])
            `evaluatesTo`
            (S.Sym "foo")
          it "evaluating x throws an unknown variable exception when no value is bound to x" $ do
            pending

evaluatesTo :: S.Sexpr -> S.Sexpr -> Expectation
evaluatesTo expr expected = do
  (result, _) <- runStateT (runEval . eval $ expr) defaultEnv
  case result of
    (Right actual)  -> actual `shouldBe` expected
    (Left err)      -> assertFailure (show err)

insertsInEnv :: S.Sexpr -> (String, S.Sexpr) -> Expectation
insertsInEnv expr (key, expected) = do
  (_, env) <- runStateT (runEval . eval $ expr) defaultEnv
  case M.lookup key env of
    Just actual -> actual `shouldBe` expected
    Nothing     -> assertFailure $ "Variable " ++ key ++ "not found in env"
