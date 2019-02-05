module Spec.Eval (run) where

import Control.Monad.State (runStateT)
import qualified Data.Map as M
import Test.Hspec (hspec, describe, it, pending, shouldBe)
import Test.Hspec.Expectations (Expectation)
import Test.HUnit.Lang (assertFailure)

import Syntax (Sexpr(..), SpecialForm(..), defaultEnv)
import Eval (EvalErr(..), eval, runEval)

run :: IO ()
run = hspec $ do
  describe "Eval" $ do
    describe "Eval.eval" $ do
      describe "special forms" $ do
        describe "atom?" $ do
          it "(atom? x) evaluates to true when x is an atom" $ do
            (Lst [SFrm IsAtm, Sym "x"]) `evaluatesTo` (Sym "true")
          it "(atom? x) evaluates to false when x is not an atom" $ do
            (Lst [SFrm IsAtm, Lst []])
            `evaluatesTo`
            (Sym "false")
          it "(atom? x y) fails with NumArgs" $ do
            (Lst [SFrm IsAtm, Sym "x", Sym "y"]) `failsWith` NumArgs

        describe "car" $ do
          it "(car xs) returns the first item in xs" $ do
            (Lst [SFrm Car, Lst [Sym "x"]]) `evaluatesTo` (Sym "x")
          it "(car xs) fails with WrongTipe if xs is not a list" $ do
            (Lst [SFrm Car, Sym "x"]) `failsWith` WrongTipe
          it "(car xs) fails with LstLength if xs is an empty list" $ do
            (Lst [SFrm Car, Lst []]) `failsWith` LstLength

        describe "cdr" $ do
          it "(cdr xs) returns the tail of xs" $ do
            (Lst [SFrm Cdr, Lst [Sym "x", Sym "y", Sym "z"]])
            `evaluatesTo`
            (Lst [Sym "y", Sym "z"])
          it "(cdr xs) returns an empty list if xs is empty" $ do
            (Lst [SFrm Cdr, Lst []]) `failsWith` LstLength
          it "(car xs) throws an exception if xs is not a list" $ do
            (Lst [SFrm Cdr, Sym "foo"]) `failsWith` WrongTipe

        describe "cond" $ do
          it "(cond cs) returns the value of the first true condition in cs" $ do
            (Lst [ SFrm Cond
                 , Lst [ Lst [ SFrm Quote, Sym "false" ]
                       , Lst [ SFrm Quote, Sym "x" ]
                       ]
                 , Lst [ Lst [ SFrm Quote, Sym "true" ]
                       , Lst [ SFrm Quote, Sym "y" ]
                       ]
                 ])
            `evaluatesTo`
            (Sym "y")
          it "(cond (quote ())) fails with NotPair" $ do
            (Lst [SFrm Cond, Lst []])
            `failsWith`
            NotPair
          it "(cond ((quote x))) fails with NotPair" $ do
            (Lst [SFrm Cond
                 , Lst [Lst [SFrm Quote, Sym "x"]]
                 ])
            `failsWith`
            NotPair
          it "(cond ((quote x) (quote y) (quote z))) fails with NotPair" $ do
            (Lst [SFrm Cond
                 , Lst [Lst [SFrm Quote, Sym "x"]]
                 , Lst [Lst [SFrm Quote, Sym "y"]]
                 , Lst [Lst [SFrm Quote, Sym "z"]]
                 ])
            `failsWith`
            NotPair
          it "(cond cs) throws an exception if cs is not a list" $ do
            (Lst [ SFrm Cond
                 , Lst [ Lst [SFrm Quote, Sym "x"]
                       , Lst [SFrm Quote, Sym "y"]
                       ]
                 ])
            `failsWith`
            WrongTipe

        describe "cons" $ do
          it "(cons x y) adds item x to the head of list y" $ do
            (Lst [ SFrm Cons
                 , Lst [ SFrm Quote, Sym "x" ]
                 , Lst [ SFrm Quote, Lst [Sym "y", Sym "z"]]])
            `evaluatesTo`
            (Lst [Sym "x", Sym "y", Sym "z"])
          it "(cons x y) returns a list of one if y is an empty list" $ do
            (Lst [ SFrm Cons
                 , Lst [ SFrm Quote, Sym "x" ]
                 , Lst [ SFrm Quote, Lst []]])
            `evaluatesTo`
            (Lst [Sym "x"])
          it "(cons (quote x) (quote y)) fails with WrongTipe" $ do
            (Lst [ SFrm Cons
                 , Lst [ SFrm Quote, Sym "x" ]
                 , Lst [ SFrm Quote, Sym "y" ]
                 ])
            `failsWith`
            WrongTipe
          it "(cons x y z) fails with NumArgs" $ do
            (Lst [ SFrm Cons
                 , Lst [ SFrm Quote, Sym "x" ]
                 , Lst [ SFrm Quote, Sym "y" ]
                 , Lst [ SFrm Quote, Sym "y" ]
                 ])
            `failsWith`
            NumArgs

        describe "define" $ do
          it "(define foo y) binds the value of y to the symbol foo" $ do
            (Lst [ SFrm Def
                 , Sym "foo"
                 , Lst [ SFrm Quote, Sym "x" ]
                 , Sym "foo" ])
            `insertsInEnv`
            ("foo", Sym "x")
          it "(define foo y) overrides the value bound to an existing symbol foo" $ do
            (Lst [ SFrm Def
                 , Sym "foo"
                 , Lst [ SFrm Quote, Sym "x" ]
                 , Lst [ SFrm Def
                       , Sym "foo"
                       , Lst [ SFrm Quote, Sym "y" ]
                       , Sym "foo" ]])
            `insertsInEnv`
            ("foo", Sym "y")
          it "(define '(x) 'x 'y) fails with WrongTipe" $ do
            (Lst [ SFrm Def
                 , Lst [ SFrm Quote, Lst [ Sym "x" ] ]
                 , Lst [ SFrm Quote, Sym "x" ]
                 , Sym "x"
                 ])
            `failsWith`
            WrongTipe
          it "(define x) fails with NumArgs" $ do
            (Lst [ SFrm Def
                 , Lst [ Sym "x" ]
                 ])
            `failsWith`
            NumArgs

        describe "eq?" $ do
          it "(eq x y) evaluates to true if x and y are equivalent symbols" $ do
            (Lst [SFrm IsEq, Sym "x", Sym "x"])
            `evaluatesTo`
            (Sym "true")
          it "(eq x y) evaluates to false if x and y are not equivalent symbols" $ do
            (Lst [SFrm IsEq, Sym "x", Sym "y"])
            `evaluatesTo`
            (Sym "false")
          it "(eq x y) evaluates to true x and y are empty lists" $ do
            (Lst [SFrm IsEq, Lst [], Lst []])
            `evaluatesTo`
            (Sym "true")
          it "(eq x y) evaluates to false" $ do
            (Lst [SFrm IsEq, Lst [ Sym "x" ], Lst [ Sym "x" ]])
            `evaluatesTo`
            (Sym "false")
          it "(eq x) fails with NumArgs" $ do
            (Lst [SFrm IsEq, Sym "x"])
            `failsWith`
            NumArgs
          it "(eq x y z) fails with NumArgs" $ do
            (Lst [SFrm IsEq, Sym "x", Sym "y", Sym "z"])
            `failsWith`
            NumArgs

        describe "lambda" $ do
          it "(lambda (x y) x) returns a function" $ do
            (Lst [SFrm Lambda, Lst [Sym "x", Sym "y"], Sym "x"])
            `evaluatesTo`
            (Fn defaultEnv [Sym "x", Sym "y"] (Sym "x"))
          it "(lambda (x y)) fails with NumArgs" $ do
            (Lst [SFrm Lambda, Lst [Sym "x", Sym "y"]])
            `failsWith`
            NumArgs
          it "(lambda (x y) z a) fails with NumArgs" $ do
            (Lst [SFrm Lambda, Lst [Sym "x", Sym "y"], Sym "z", Sym "a"])
            `failsWith`
            NumArgs
          it "(lambda x y) fails with WrongTipe" $ do
            (Lst [SFrm Lambda, Sym "x", Sym "y"])
            `failsWith`
            WrongTipe

        describe "quote" $ do
          it "(quote (x, y)) evaluates to (x y)" $ do
            (Lst [SFrm Quote, Lst [Sym "x", Sym "y"]])
            `evaluatesTo`
            (Lst [Sym "x", Sym "y"])
          it "(quote x) evaluates to the symbol x" $ do
            (Lst [SFrm Quote, Sym "x"]) `evaluatesTo` (Sym "x")
          it "(quote x y) fails with NumArgs" $ do
            (Lst [SFrm Quote, Sym "x", Sym "y"])
            `failsWith`
            NumArgs

      describe "function application" $ do
          it "((lambda (x) x) (quote y)) evaluates to the symbol y" $ do
            (Lst [ Lst [ SFrm Lambda, Lst [ Sym "x" ]
                       , Sym "x" ]
                 , Lst [ SFrm Quote, Sym "y" ]])
            `evaluatesTo`
            (Sym "y")
          it "(define x (quote y) (y (quote z)) fails with NotFn" $ do
            (Lst [ Lst [ SFrm Quote , Sym "x" ]
                 , Lst [ SFrm Quote, Sym "y" ]
                 ])
            `failsWith`
            NotFn

      describe "variable lookup" $ do
          it "evaluating x returns the value bound to the symbol x" $ do
            (Lst [ SFrm Def
                 , Sym "foo"
                 , Lst [ SFrm Quote, Sym "x" ]
                 , Sym "foo" ])
            `evaluatesTo`
            (Sym "x")
          it "evaluating x throws an unknown variable exception when no value is bound to x" $ do
            (Sym "x") `failsWith` UnknownVar

evaluatesTo :: Sexpr -> Sexpr -> Expectation
evaluatesTo expr expected = do
  (result, _) <- runStateT (runEval . eval $ expr) defaultEnv
  case result of
    (Right actual)  -> actual `shouldBe` expected
    (Left err)      -> assertFailure (show err)

failsWith :: Sexpr -> EvalErr -> Expectation
failsWith expr expected = do
  (result, _) <- runStateT (runEval . eval $ expr) defaultEnv
  case result of
    (Right x)     -> assertFailure $ "expected evaluation to fail but received: " ++ (show x)
    (Left actual) -> actual `shouldBe` expected

insertsInEnv :: Sexpr -> (String, Sexpr) -> Expectation
insertsInEnv expr (key, expected) = do
  (_, env) <- runStateT (runEval . eval $ expr) defaultEnv
  case M.lookup key env of
    Just actual -> actual `shouldBe` expected
    Nothing     -> assertFailure $ "Variable " ++ key ++ "not found in env"
