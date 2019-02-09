module Spec.Eval (runTests) where

import qualified Data.Map as M
import Test.Hspec (hspec, describe, it, shouldBe)
import Test.Hspec.Expectations (Expectation)
import Test.HUnit.Lang (assertFailure)
import Test.QuickCheck (Arbitrary, Gen, Property, arbitrary, elements, property, resize)
import Test.QuickCheck.Instances.Char (lowerAlpha, numeric, upperAlpha)
import Test.QuickCheck.Gen (oneof, listOf)
import qualified Test.QuickCheck.Monadic as Monadic
import qualified Syntax as Sx
import Syntax (Env, Expr(..), Info(..), Sexpr(..), SpecialForm(..), defaultEnv)
import Eval (EvalErr(..), run)

runTests :: IO ()
runTests = hspec $ do
  describe "Eval" $ do
    describe "Eval.eval" $ do
      describe "special forms" $ do
        describe "atom?" $ do
          it "(atom? x) evaluates to true when x is an atom" $ do
            (toExpr $ Lst [ toExpr $ SFrm IsAtm
                          , toExpr $ Lst [ toExpr $ SFrm Quote
                                         , toExpr $ Sym "x"
                                         ]
                          ])
            `evaluatesTo`
            (toExpr $ Sym "t")
          it "(atom? x) evaluates to true when x is an empty list" $ do
            (toExpr $ Lst [ toExpr $ SFrm IsAtm
                          , toExpr $ Lst [ toExpr $ SFrm Quote
                                         , toExpr $ Lst []
                                         ]
                 ])
            `evaluatesTo`
            (toExpr $ Sym "t")
          it "(atom? x) evaluates to false when x is a non-empty list" $ do
            (toExpr $ Lst [ toExpr $ SFrm IsAtm
                          , toExpr $ Lst [ toExpr $ SFrm Quote
                                         , toExpr $ Lst [ toExpr $ Sym "foo" ]
                                         ]
                          ])
            `evaluatesTo`
            (toExpr $ Lst [])
          it "(atom? x y) fails with NumArgs" $ do
            (toExpr $ Lst [ toExpr $ SFrm IsAtm
                          , toExpr $ Lst [ toExpr $ SFrm Quote
                                         , toExpr $ Sym "x"
                                         ]
                          , toExpr $ Lst [ toExpr $ SFrm Quote
                                         , toExpr $ Sym "y"
                                         ]
                          ])
            `failsWith`
            NumArgs

        describe "car" $ do
          it "(car xs) returns the first item in xs" $ do
            (toExpr $ Lst [ toExpr $ SFrm Car
                          , toExpr $ Lst [ toExpr $ SFrm Quote
                                         , toExpr $ Lst [ toExpr $ Sym "x"]
                                         ]
                          ])
            `evaluatesTo`
            (toExpr $ Sym "x")
          it "(car xs) fails with WrongTipe if xs is not a list" $ do
            (toExpr $ Lst [ toExpr $ SFrm Car
                          , toExpr $ Lst [ toExpr $ SFrm Quote
                                         , toExpr $ Sym "x"
                                         ]
                          ])
            `failsWith`
            WrongTipe
          it "(car xs) fails with LstLength if xs is an empty list" $ do
            (toExpr $ Lst [ toExpr $ SFrm Car
                          , toExpr $ Lst [ toExpr $ SFrm Quote
                                         , toExpr $ Lst []
                                         ]
                          ])
            `failsWith`
            LstLength

        describe "cdr" $ do
          it "(cdr xs) returns the tail of xs" $ do
            (toExpr $ Lst [ toExpr $ SFrm Cdr
                          , toExpr $ Lst [ toExpr $ SFrm Quote
                                         , toExpr $ Lst [ toExpr $ Sym "x"
                                                        , toExpr $ Sym "y"
                                                        , toExpr $ Sym "z"
                                                        ]
                                         ]
                          ])
            `evaluatesTo`
            (toExpr $ Lst [ toExpr $ Sym "y", toExpr $ Sym "z" ])
          it "(cdr xs) returns an empty list if xs is empty" $ do
            (toExpr $ Lst [ toExpr $ SFrm Cdr
                          , toExpr $ Lst [ toExpr $ SFrm Quote
                                         , toExpr $ Lst []
                                         ]
                          ])
            `evaluatesTo`
            (toExpr $ Lst [])
          it "(car xs) throws an exception if xs is not a list" $ do
            (toExpr $ Lst [ toExpr $ SFrm Cdr
                          , toExpr $ Lst [ toExpr $ SFrm Quote
                                         , toExpr $ Sym "foo"
                                         ]
                          ])
            `failsWith`
            WrongTipe

        describe "cond" $ do
          it "(cond cs) returns the value of the first true condition in cs" $ do
            (toExpr $ Lst [ toExpr $ SFrm Cond
                          , toExpr $ Lst [ toExpr $ Lst [ toExpr $ SFrm Quote
                                                        , toExpr $ Lst []
                                                        ]
                                         , toExpr $ Lst [ toExpr $ SFrm Quote
                                                        , toExpr $ Sym "x"
                                                        ]
                                         ]
                          , toExpr $ Lst [ toExpr $ Lst [ toExpr $ SFrm Quote
                                                        , toExpr $ Sym "t"
                                                        ]
                                         , toExpr $ Lst [ toExpr $ SFrm Quote
                                                        , toExpr $ Sym "y"
                                                        ]
                                         ]
                          ])
            `evaluatesTo`
            (toExpr $ Sym "y")
          it "(cond (quote ())) fails with NotPair" $ do
            (toExpr $ Lst [ toExpr $ SFrm Cond
                          , toExpr $ Lst []
                          ])
            `failsWith`
            NotPair
          it "(cond ((quote x))) fails with NotPair" $ do
            (toExpr $ Lst [ toExpr $ SFrm Cond
                          , toExpr $ Lst [ toExpr $ Lst [ toExpr $ SFrm Quote
                                                        , toExpr $ Sym "x"
                                                        ]
                                         ]
                          ])
            `failsWith`
            NotPair
          it "(cond ((quote x) (quote y) (quote z))) fails with NotPair" $ do
            (toExpr $ Lst [ toExpr $ SFrm Cond
                          , toExpr $ Lst [ toExpr $ Lst [ toExpr $ SFrm Quote, toExpr $ Sym "x" ]]
                          , toExpr $ Lst [ toExpr $ Lst [ toExpr $ SFrm Quote, toExpr $ Sym "y" ]]
                          , toExpr $ Lst [ toExpr $ Lst [ toExpr $ SFrm Quote, toExpr $ Sym "z" ]]
                          ])
            `failsWith`
            NotPair
          it "(cond cs) throws an exception if cs is not a list" $ do
            (toExpr $ Lst [ toExpr $ SFrm Cond
                          , toExpr $ Lst [ toExpr $ Lst [ toExpr $ SFrm Quote, toExpr $ Sym "x"]
                                         , toExpr $ Lst [ toExpr $ SFrm Quote, toExpr $ Sym "y"]
                                         ]
                          ])
            `failsWith`
            WrongTipe

        describe "cons" $ do
          it "(cons x y) adds item x to the head of list y" $ do
            (toExpr $ Lst [ toExpr $ SFrm Cons
                          , toExpr $ Lst [ toExpr $ SFrm Quote
                                         , toExpr $ Sym "x"
                                         ]
                          , toExpr $ Lst [ toExpr $ SFrm Quote
                                         , toExpr $ Lst [ toExpr $ Sym "y"
                                                        , toExpr $ Sym "z"
                                                        ]
                                         ]
                          ])
            `evaluatesTo`
            (toExpr $ Lst [ toExpr $ Sym "x"
                          , toExpr $ Sym "y"
                          , toExpr $ Sym "z"
                          ])
          it "(cons x y) returns a list of one if y is an empty list" $ do
            (toExpr $ Lst [ toExpr $ SFrm Cons
                          , toExpr $ Lst [ toExpr $ SFrm Quote
                                         , toExpr $ Sym "x"
                                         ]
                          , toExpr $ Lst [ toExpr $ SFrm Quote
                                         , toExpr $ Lst []
                                         ]
                          ])
            `evaluatesTo`
            (toExpr $ Lst [ toExpr $ Sym "x" ])
          it "(cons (quote x) (quote y)) fails with WrongTipe" $ do
            (toExpr $ Lst [ toExpr $ SFrm Cons
                          , toExpr $ Lst [ toExpr $ SFrm Quote
                                         , toExpr $ Sym "x"
                                         ]
                          , toExpr $ Lst [ toExpr $ SFrm Quote
                                         , toExpr $ Sym "y"
                                         ]
                          ])
            `failsWith`
            WrongTipe
          it "(cons x y z) fails with NumArgs" $ do
            (toExpr $ Lst [ toExpr $ SFrm Cons
                          , toExpr $ Lst [ toExpr $ SFrm Quote, toExpr $ Sym "x" ]
                          , toExpr $ Lst [ toExpr $ SFrm Quote, toExpr $ Sym "y" ]
                          , toExpr $ Lst [ toExpr $ SFrm Quote, toExpr $ Sym "y" ]
                          ])
            `failsWith`
            NumArgs

        describe "define" $ do
          it "(define foo y) binds the value of y to the symbol foo" $ do
            (toExpr $ Lst [ toExpr $ SFrm Def
                          , toExpr $ Sym "foo"
                          , toExpr $ Lst [ toExpr $ SFrm Quote, toExpr $ Sym "x" ]
                          ])
            `insertsInEnv`
            ("foo", toExpr $ Sym "x")
          it "(define '(x) 'x 'y) fails with WrongTipe" $ do
            (toExpr $ Lst [ toExpr $  SFrm Def
                          , toExpr $ Lst [ toExpr $ SFrm Quote
                                         , toExpr $ Lst [ toExpr $ Sym "x" ]
                                         ]
                          , toExpr $ Lst [ toExpr $ SFrm Quote
                                         , toExpr $ Sym "x"
                                         ]
                          ])
            `failsWith`
            WrongTipe
          it "(define x) fails with NumArgs" $ do
            (toExpr $ Lst [ toExpr $ SFrm Def
                          , toExpr $ Lst [ toExpr $ Sym "x" ]
                          ])
            `failsWith`
            NumArgs

        describe "eq?" $ do
          it "(eq? x y) evaluates to true if x and y are equivalent symbols" $ do
            (toExpr $ Lst [ toExpr $ SFrm IsEq
                          , toExpr $ Lst [ toExpr $ SFrm Quote, toExpr $ Sym "x" ]
                          , toExpr $ Lst [ toExpr $ SFrm Quote, toExpr $ Sym "x" ]
                          ])
            `evaluatesTo`
            (toExpr $ Sym "t")
          it "(eq? x y) evaluates to false if x and y are not equivalent symbols" $ do
            (toExpr $ Lst [ toExpr $ SFrm IsEq
                          , toExpr $ Lst [ toExpr $ SFrm Quote, toExpr $ Sym "x" ]
                          , toExpr $ Lst [ toExpr $ SFrm Quote, toExpr $ Sym "y" ]
                          ])
            `evaluatesTo`
            (toExpr $ Lst [])
          it "(eq? x y) evaluates to true x and y are empty lists" $ do
            (toExpr $ Lst [ toExpr $ SFrm IsEq
                          , toExpr $ Lst [ toExpr $ SFrm Quote, toExpr $ Lst [] ]
                          , toExpr $ Lst [ toExpr $ SFrm Quote, toExpr $ Lst [] ]
                          ])
            `evaluatesTo`
            (toExpr $ Sym "t")
          it "(eq? x y) evaluates to false" $ do
            (toExpr $ Lst [ toExpr $  SFrm IsEq
                 , toExpr $ Lst [ toExpr $ SFrm Quote, toExpr $ Lst [ toExpr $ Sym "x" ] ]
                 , toExpr $ Lst [ toExpr $ SFrm Quote, toExpr $ Lst [ toExpr $ Sym "x" ] ]
                 ])
            `evaluatesTo`
            (toExpr $ Lst [])
          it "(eq? x) fails with NumArgs" $ do
            (toExpr $ Lst [ toExpr $ SFrm IsEq, toExpr $ Sym "x"])
            `failsWith`
            NumArgs
          it "(eq? x y z) fails with NumArgs" $ do
            (toExpr $ Lst [ toExpr $ SFrm IsEq
                          , toExpr $ Sym "x"
                          , toExpr $ Sym "y"
                          , toExpr $ Sym "z"
                          ])
            `failsWith`
            NumArgs

        describe "lambda" $ do
          it "(lambda (x y) x) returns a function" $ do
            (toExpr $ Lst [ toExpr $ SFrm Lambda
                          , toExpr $ Lst [ toExpr $ Sym "x"
                                         , toExpr $ Sym "y"
                                         ]
                          , toExpr $ Sym "x"
                          ])
            `evaluatesTo`
            (toExpr $ Fn defaultEnv [ toExpr $ Sym "x", toExpr $ Sym "y"] (toExpr $ Sym "x"))
          it "(lambda (x y)) fails with NumArgs" $ do
            (toExpr $ Lst [ toExpr $ SFrm Lambda
                          , toExpr $ Lst [ toExpr $ Sym "x"
                                         , toExpr $ Sym "y"
                                         ]
                          ])
            `failsWith`
            NumArgs
          it "(lambda (x y) z a) fails with NumArgs" $ do
            (toExpr $ Lst [ toExpr $ SFrm Lambda
                          , toExpr $ Lst [ toExpr $ Sym "x"
                                         , toExpr $ Sym "y"
                                         ]
                          , toExpr $ Sym "z"
                          , toExpr $ Sym "a"
                          ])
            `failsWith`
            NumArgs
          it "(lambda x y) fails with WrongTipe" $ do
            (toExpr $ Lst [ toExpr $ SFrm Lambda
                          , toExpr $ Sym "x"
                          , toExpr $ Sym "y"
                          ])
            `failsWith`
            WrongTipe

        describe "quote" $ do
          it "(quote <lisp expression>) evaluates to <lisp expression>" $ do
            property prop_eval_quote
          it "(quote x) evaluates to the symbol x" $ do
            (toExpr $ Lst [ toExpr $ SFrm Quote
                          , toExpr $ Sym "x"
                          ])
            `evaluatesTo`
            (toExpr $ Sym "x")
          it "(quote x y) fails with NumArgs" $ do
            (toExpr $ Lst [ toExpr $ SFrm Quote
                          , toExpr $ Sym "x"
                          , toExpr $ Sym "y"
                          ])
            `failsWith`
            NumArgs

      describe "function application" $ do
          it "((lambda (x) x) (quote y)) evaluates to the symbol y" $ do
            (toExpr $ Lst [ toExpr $ Lst [ toExpr $ SFrm Lambda
                                         , toExpr $ Lst [ toExpr $ Sym "x" ]
                                         , toExpr $ Sym "x"
                                         ]
                 , toExpr $ Lst [ toExpr $ SFrm Quote
                                , toExpr $ Sym "y"
                                ]
                          ])
            `evaluatesTo`
            (toExpr $ Sym "y")
          it "(define x (quote y) (y (quote z)) fails with NotFn" $ do
            (toExpr $ Lst [ toExpr $ Lst [ toExpr $ SFrm Quote
                                         , toExpr $ Sym "x"
                                         ]
                          , toExpr $ Lst [ toExpr $ SFrm Quote
                                         , toExpr $ Sym "y"
                                         ]
                          ])
            `failsWith`
            NotFn

      describe "variable lookup" $ do
          it "evaluating x returns the value bound to the symbol x" $ do
            inEnvEvaluatesTo
              (M.fromList [("foo", toExpr $ Sym "x")] <> defaultEnv)
              (toExpr $ Sym "foo")
              (toExpr $ Sym "x")
          it "evaluating x throws an unknown variable exception when no value is bound to x" $ do
            (toExpr $ Sym "x") `failsWith` (UnknownVar "x")

inEnvEvaluatesTo :: Env -> Expr -> Expr -> Expectation
inEnvEvaluatesTo env expr expected = do
  (result, _) <- Eval.run env expr
  case result of
    (Right actual)  -> actual `shouldBe` expected
    (Left err)      -> assertFailure (show err)

evaluatesTo :: Expr -> Expr -> Expectation
evaluatesTo expr expected = do
  (result, _) <- Eval.run defaultEnv expr
  case result of
    (Right actual)  -> actual `shouldBe` expected
    (Left err)      -> assertFailure (show err)

failsWith :: Expr -> EvalErr -> Expectation
failsWith expr expected = do
  (result, _) <- Eval.run defaultEnv expr
  case result of
    (Right x)     -> assertFailure $ "expected evaluation to fail but received: " ++ (show x)
    (Left actual) -> actual `shouldBe` expected

insertsInEnv :: Expr -> (String, Expr) -> Expectation
insertsInEnv expr (key, expected) = do
  (_, env) <- Eval.run defaultEnv expr
  case M.lookup key env of
    Just actual -> actual `shouldBe` expected
    Nothing     -> assertFailure $ "Variable " ++ key ++ "not found in env"

prop_eval_quote :: ArbExpr -> Property
prop_eval_quote (ArbExpr expr) = Monadic.monadicIO $ do
  (result, _) <- Monadic.run $ Eval.run defaultEnv quotedExpr
  case result of
    (Right actual) -> Monadic.assert (actual == expr)
    _              -> Monadic.assert False
  where quotedExpr = toExpr $ Lst [ toExpr $ SFrm Quote, expr ]

toExpr :: Sexpr -> Expr
toExpr sexpr = Expr sexpr dummyInfo

dummyInfo :: Info
dummyInfo = Info 0 0 ""

newtype ArbExpr = ArbExpr Expr deriving (Eq, Show)

instance Arbitrary ArbExpr where
  arbitrary = ArbExpr <$> oneof [ genSym, genSFrm, genLst ]

genSym :: Gen Expr
genSym = do
  name <- listOf $ oneof [ numeric, lowerAlpha, upperAlpha ]
  return $ toExpr (Sym name)

genSFrm :: Gen Expr
genSFrm = (toExpr . SFrm) <$> elements [ Car , Cdr , Cons , Cond , Def , IsAtm , IsEq , Lambda , Quote ]

genLst :: Gen Expr
genLst = do
  sexprs <- listOf $ oneof [ genSym, genSFrm, resize 2 genLst ]
  return $ toExpr (Lst sexprs)
