module Spec.Eval (run) where

import Control.Monad.State (runStateT)
import qualified Data.Map as M
import Test.Hspec (hspec, describe, it, pending, shouldBe)
import Test.Hspec.Expectations (Expectation)
import Test.HUnit.Lang (assertFailure)

import Syntax (Sexpr(..), defaultEnv)
import Eval (eval, runEval)

run :: IO ()
run = hspec $ do
  describe "Eval" $ do
    describe "Eval.eval" $ do

      describe "special forms" $ do
        describe "atom?" $ do
          it "(atom? x) evaluates to true when x is an atom" $ do
            (Lst [Sym "atom?", Sym "x"]) `evaluatesTo` (Sym "true")
          it "(atom? x) evaluates to false when x is not an atom" $ do
            (Lst [Sym "atom?", Lst []])
            `evaluatesTo`
            (Sym "false")

        describe "car" $ do
          it "(car xs) returns the first item in xs" $ do
            (Lst [Sym "car", Lst [Sym "x"]]) `evaluatesTo` (Sym "x")
          it "(car xs) throws an exception if xs is not a list" $ do
            pending

        describe "cdr" $ do
          it "(cdr xs) returns the tail of xs" $ do
            (Lst [Sym "cdr", Lst [Sym "x", Sym "y", Sym "z"]])
            `evaluatesTo`
            (Lst [Sym "y", Sym "z"])
          it "(cdr xs) returns an empty list if xs is empty" $ do
            (Lst [Sym "cdr", Lst []])
            `evaluatesTo`
            (Lst [])
          it "(car xs) throws an exception if xs is not a list" $ do
            pending

        describe "cond" $ do
          it "(cond cs) returns the value of the first true condition in cs" $ do
            (Lst [ Sym "cond"
                   , Lst [ Lst [ Lst [ Sym "quote", Sym "false" ]
                                     , Lst [ Sym "quote", Sym "x" ]]
                         , Lst [ Lst [ Sym "quote", Sym "true" ]
                                     , Lst [ Sym "quote", Sym "y" ]]]])
            `evaluatesTo`
            (Sym "y")
          it "(cond cs) throws an exception if cs is an empty list" $ do
            pending
          it "(cond cs) throws an exception if cs is not a list" $ do
            pending

        describe "cons" $ do
          it "(cons x y) adds item x to the head of list y" $ do
            (Lst [ Sym "cons"
                 , Lst [ Sym "quote", Sym "x" ]
                 , Lst [ Sym "quote", Lst [Sym "y", Sym "z"]]])
            `evaluatesTo`
            (Lst [Sym "x", Sym "y", Sym "z"])
          it "(cons x y) returns a list of one if y is an empty list" $ do
            (Lst [ Sym "cons"
                 , Lst [ Sym "quote", Sym "x" ]
                 , Lst [ Sym "quote", Lst []]])
            `evaluatesTo`
            (Lst [Sym "x"])
          it "(cons x y) throws an exception if y is not a list" $ do
            pending
          it "(cons x) throws an exception" $ do
            pending

        describe "define" $ do
          it "(define foo y) binds the value of y to the symbol foo" $ do
            (Lst [ Sym "define"
                 , Sym "foo"
                 , Lst [ Sym "quote", Sym "x" ]
                 , Sym "foo" ])
            `insertsInEnv`
            ("foo", Sym "x")
          it "(define foo y) overrides the value bound to an existing symbol foo" $ do
            (Lst [ Sym "define"
                 , Sym "foo"
                 , Lst [ Sym "quote", Sym "x" ]
                 , Lst [ Sym "define"
                       , Sym "foo"
                       , Lst [ Sym "quote", Sym "y" ]
                       , Sym "foo" ]])
            `insertsInEnv`
            ("foo", Sym "y")
          it "(define foo) throws an exception" $ do
            pending

        describe "eq?" $ do
          it "(eq x y) returns true if x and y are equivalent symbols" $ do
            (Lst [Sym "eq?", Sym "x", Sym "x"])
            `evaluatesTo`
            (Sym "true")
          it "(eq x y) returns false if x and y are not equivalent symbols" $ do
            (Lst [Sym "eq?", Sym "x", Sym "y"])
            `evaluatesTo`
            (Sym "false")
          it "(eq x y) throws an exception if x and y are not symbols" $ do
            pending

        describe "lambda" $ do
          it "(lambda (x y) x) returns a function" $ do
            (Lst [Sym "lambda", Lst [Sym "x", Sym "y"], Sym "x"])
            `evaluatesTo`
            (Lamd defaultEnv [Sym "x", Sym "y"] (Sym "x"))
          it "(lambda (x y)) throws an exception" $ do
            pending
          it "(lambda (x y)) throws an argument number exception" $ do
            pending
          it "(lambda x y) throws type error exception" $ do
            pending

        describe "quote" $ do
          it "(quote (x, y)) evaluates to (x y)" $ do
            (Lst [Sym "quote", Lst [Sym "x", Sym "y"]])
            `evaluatesTo`
            (Lst [Sym "x", Sym "y"])
          it "(quote x) evaluates to the symbol x" $ do
            (Lst [Sym "quote", Sym "x"]) `evaluatesTo` (Sym "x")
          it "(quote x y z) throws an argument exception" $ do
            pending

      describe "function application" $ do
          it "((lambda (x) x) (quote y)) evaluates to the symbol y" $ do
            (Lst [ Lst [ Sym "lambda", Lst [ Sym "x" ]
                       , Sym "x" ]
                 , Lst [ Sym "quote", Sym "y" ]])
            `evaluatesTo`
            (Sym "y")
          it "((lambda (x y) y) (quote z)) throws an arguments exception" $ do
            pending

      describe "variable lookup" $ do
          it "evaluating x returns the value bound to the symbol x" $ do
            (Lst [ Sym "define"
                 , Sym "foo"
                 , Lst [ Sym "quote", Sym "x" ]
                 , Sym "foo" ])
            `evaluatesTo`
            (Sym "x")
          it "evaluating x throws an unknown variable exception when no value is bound to x" $ do
            pending

evaluatesTo :: Sexpr -> Sexpr -> Expectation
evaluatesTo expr expected = do
  (result, _) <- runStateT (runEval . eval $ expr) defaultEnv
  case result of
    (Right actual)  -> actual `shouldBe` expected
    (Left err)      -> assertFailure (show err)

insertsInEnv :: Sexpr -> (String, Sexpr) -> Expectation
insertsInEnv expr (key, expected) = do
  (_, env) <- runStateT (runEval . eval $ expr) defaultEnv
  case M.lookup key env of
    Just actual -> actual `shouldBe` expected
    Nothing     -> assertFailure $ "Variable " ++ key ++ "not found in env"
