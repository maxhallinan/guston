module Spec.Eval (runTests) where

import qualified Data.Map as M
import Eval (ErrType (..), EvalErr (..), run)
import Syntax (Env, Expr (..), Info (..), SpecialForm (..), XExpr (..), defaultEnv)
import Test.Hspec (describe, hspec, it, shouldBe)
import Test.Hspec.Expectations (Expectation)
import Test.HUnit.Lang (assertFailure)
import Test.QuickCheck (Arbitrary, Gen, Property, arbitrary, elements, property, resize)
import Test.QuickCheck.Gen (listOf, oneof)
import Test.QuickCheck.Instances.Char (lowerAlpha, numeric, upperAlpha)
import qualified Test.QuickCheck.Monadic as Monadic

runTests :: IO ()
runTests = hspec $ do
  describe "Eval" $ do
    describe "Eval.eval" $ do
      describe "special forms" $ do
        describe "atom?" $ do
          it "(atom? x) evaluates to true when x is an atom" $ do
            (toXExpr $ Lst [ toXExpr $ SFrm IsAtm
                           , toXExpr $ Lst [ toXExpr $ SFrm Quote
                                         , toXExpr $ Sym "x"
                                         ]
                          ])
            `evaluatesTo`
            (toXExpr $ Sym "t")
          it "(atom? x) evaluates to true when x is an empty list" $ do
            (toXExpr $ Lst [ toXExpr $ SFrm IsAtm
                          , toXExpr $ Lst [ toXExpr $ SFrm Quote
                                         , toXExpr $ Lst []
                                         ]
                 ])
            `evaluatesTo`
            (toXExpr $ Sym "t")
          it "(atom? x) evaluates to false when x is a non-empty list" $ do
            (toXExpr $ Lst [ toXExpr $ SFrm IsAtm
                          , toXExpr $ Lst [ toXExpr $ SFrm Quote
                                         , toXExpr $ Lst [ toXExpr $ Sym "foo" ]
                                         ]
                          ])
            `evaluatesTo`
            (toXExpr $ Lst [])
          it "(atom? x y) fails with NumArgs" $ do
            (toXExpr $ Lst [ toXExpr $ SFrm IsAtm
                          , toXExpr $ Lst [ toXExpr $ SFrm Quote
                                         , toXExpr $ Sym "x"
                                         ]
                          , toXExpr $ Lst [ toXExpr $ SFrm Quote
                                         , toXExpr $ Sym "y"
                                         ]
                          ])
            `failsWith`
            NumArgs

        describe "car" $ do
          it "(car xs) returns the first item in xs" $ do
            (toXExpr $ Lst [ toXExpr $ SFrm First
                          , toXExpr $ Lst [ toXExpr $ SFrm Quote
                                         , toXExpr $ Lst [ toXExpr $ Sym "x"]
                                         ]
                          ])
            `evaluatesTo`
            (toXExpr $ Sym "x")
          it "(car xs) fails with WrongTipe if xs is not a list" $ do
            (toXExpr $ Lst [ toXExpr $ SFrm First
                          , toXExpr $ Lst [ toXExpr $ SFrm Quote
                                         , toXExpr $ Sym "x"
                                         ]
                          ])
            `failsWith`
            WrongTipe
          it "(car xs) fails with LstLength if xs is an empty list" $ do
            (toXExpr $ Lst [ toXExpr $ SFrm First
                          , toXExpr $ Lst [ toXExpr $ SFrm Quote
                                         , toXExpr $ Lst []
                                         ]
                          ])
            `failsWith`
            LstLength

        describe "cdr" $ do
          it "(cdr xs) returns the tail of xs" $ do
            (toXExpr $ Lst [ toXExpr $ SFrm Rest
                          , toXExpr $ Lst [ toXExpr $ SFrm Quote
                                         , toXExpr $ Lst [ toXExpr $ Sym "x"
                                                        , toXExpr $ Sym "y"
                                                        , toXExpr $ Sym "z"
                                                        ]
                                         ]
                          ])
            `evaluatesTo`
            (toXExpr $ Lst [ toXExpr $ Sym "y", toXExpr $ Sym "z" ])
          it "(cdr xs) returns an empty list if xs is empty" $ do
            (toXExpr $ Lst [ toXExpr $ SFrm Rest
                          , toXExpr $ Lst [ toXExpr $ SFrm Quote
                                         , toXExpr $ Lst []
                                         ]
                          ])
            `evaluatesTo`
            (toXExpr $ Lst [])
          it "(car xs) throws an exception if xs is not a list" $ do
            (toXExpr $ Lst [ toXExpr $ SFrm Rest
                          , toXExpr $ Lst [ toXExpr $ SFrm Quote
                                         , toXExpr $ Sym "foo"
                                         ]
                          ])
            `failsWith`
            WrongTipe

        describe "if" $ do
          it "(if (quote t) (quote x) (quote y)) evaluates to x" $ do
            (toXExpr $ Lst [ toXExpr $ SFrm If
                          , toXExpr $ Lst [ toXExpr $ SFrm Quote, toXExpr $ Sym "t" ]
                          , toXExpr $ Lst [ toXExpr $ SFrm Quote, toXExpr $ Sym "x" ]
                          , toXExpr $ Lst [ toXExpr $ SFrm Quote, toXExpr $ Sym "y" ]
                          ])
            `evaluatesTo`
            (toXExpr $ Sym "x")
          it "(if (quote ()) (quote x) (quote y)) evaluates to y" $ do
            (toXExpr $ Lst [ toXExpr $ SFrm If
                          , toXExpr $ Lst [ toXExpr $ SFrm Quote, toXExpr $ Lst [] ]
                          , toXExpr $ Lst [ toXExpr $ SFrm Quote, toXExpr $ Sym "x" ]
                          , toXExpr $ Lst [ toXExpr $ SFrm Quote, toXExpr $ Sym "y" ]
                          ])
            `evaluatesTo`
            (toXExpr $ Sym "y")
          it "(cond (quote ())) fails with NumArgs" $ do
            (toXExpr $ Lst [ toXExpr $ SFrm If
                          , toXExpr $ Sym "foo"
                          ])
            `failsWith`
            NumArgs

        describe "cons" $ do
          it "(cons x y) adds item x to the head of list y" $ do
            (toXExpr $ Lst [ toXExpr $ SFrm Cons
                          , toXExpr $ Lst [ toXExpr $ SFrm Quote
                                         , toXExpr $ Sym "x"
                                         ]
                          , toXExpr $ Lst [ toXExpr $ SFrm Quote
                                         , toXExpr $ Lst [ toXExpr $ Sym "y"
                                                        , toXExpr $ Sym "z"
                                                        ]
                                         ]
                          ])
            `evaluatesTo`
            (toXExpr $ Lst [ toXExpr $ Sym "x"
                          , toXExpr $ Sym "y"
                          , toXExpr $ Sym "z"
                          ])
          it "(cons x y) returns a list of one if y is an empty list" $ do
            (toXExpr $ Lst [ toXExpr $ SFrm Cons
                          , toXExpr $ Lst [ toXExpr $ SFrm Quote
                                         , toXExpr $ Sym "x"
                                         ]
                          , toXExpr $ Lst [ toXExpr $ SFrm Quote
                                         , toXExpr $ Lst []
                                         ]
                          ])
            `evaluatesTo`
            (toXExpr $ Lst [ toXExpr $ Sym "x" ])
          it "(cons (quote x) (quote y)) fails with WrongTipe" $ do
            (toXExpr $ Lst [ toXExpr $ SFrm Cons
                          , toXExpr $ Lst [ toXExpr $ SFrm Quote
                                         , toXExpr $ Sym "x"
                                         ]
                          , toXExpr $ Lst [ toXExpr $ SFrm Quote
                                         , toXExpr $ Sym "y"
                                         ]
                          ])
            `failsWith`
            WrongTipe
          it "(cons x y z) fails with NumArgs" $ do
            (toXExpr $ Lst [ toXExpr $ SFrm Cons
                          , toXExpr $ Lst [ toXExpr $ SFrm Quote, toXExpr $ Sym "x" ]
                          , toXExpr $ Lst [ toXExpr $ SFrm Quote, toXExpr $ Sym "y" ]
                          , toXExpr $ Lst [ toXExpr $ SFrm Quote, toXExpr $ Sym "y" ]
                          ])
            `failsWith`
            NumArgs

        describe "define" $ do
          it "(define foo y) binds the value of y to the symbol foo" $ do
            (toXExpr $ Lst [ toXExpr $ SFrm Def
                          , toXExpr $ Sym "foo"
                          , toXExpr $ Lst [ toXExpr $ SFrm Quote, toXExpr $ Sym "x" ]
                          ])
            `insertsInEnv`
            ("foo", toXExpr $ Sym "x")
          it "(define '(x) 'x 'y) fails with WrongTipe" $ do
            (toXExpr $ Lst [ toXExpr $  SFrm Def
                          , toXExpr $ Lst [ toXExpr $ SFrm Quote
                                         , toXExpr $ Lst [ toXExpr $ Sym "x" ]
                                         ]
                          , toXExpr $ Lst [ toXExpr $ SFrm Quote
                                         , toXExpr $ Sym "x"
                                         ]
                          ])
            `failsWith`
            WrongTipe
          it "(define x) fails with NumArgs" $ do
            (toXExpr $ Lst [ toXExpr $ SFrm Def
                          , toXExpr $ Lst [ toXExpr $ Sym "x" ]
                          ])
            `failsWith`
            NumArgs

        describe "eq?" $ do
          it "(eq? x y) evaluates to true if x and y are equivalent symbols" $ do
            (toXExpr $ Lst [ toXExpr $ SFrm IsEq
                          , toXExpr $ Lst [ toXExpr $ SFrm Quote, toXExpr $ Sym "x" ]
                          , toXExpr $ Lst [ toXExpr $ SFrm Quote, toXExpr $ Sym "x" ]
                          ])
            `evaluatesTo`
            (toXExpr $ Sym "t")
          it "(eq? x y) evaluates to false if x and y are not equivalent symbols" $ do
            (toXExpr $ Lst [ toXExpr $ SFrm IsEq
                          , toXExpr $ Lst [ toXExpr $ SFrm Quote, toXExpr $ Sym "x" ]
                          , toXExpr $ Lst [ toXExpr $ SFrm Quote, toXExpr $ Sym "y" ]
                          ])
            `evaluatesTo`
            (toXExpr $ Lst [])
          it "(eq? x y) evaluates to true x and y are empty lists" $ do
            (toXExpr $ Lst [ toXExpr $ SFrm IsEq
                          , toXExpr $ Lst [ toXExpr $ SFrm Quote, toXExpr $ Lst [] ]
                          , toXExpr $ Lst [ toXExpr $ SFrm Quote, toXExpr $ Lst [] ]
                          ])
            `evaluatesTo`
            (toXExpr $ Sym "t")
          it "(eq? x y) evaluates to false" $ do
            (toXExpr $ Lst [ toXExpr $  SFrm IsEq
                 , toXExpr $ Lst [ toXExpr $ SFrm Quote, toXExpr $ Lst [ toXExpr $ Sym "x" ] ]
                 , toXExpr $ Lst [ toXExpr $ SFrm Quote, toXExpr $ Lst [ toXExpr $ Sym "x" ] ]
                 ])
            `evaluatesTo`
            (toXExpr $ Lst [])
          it "(eq? x) fails with NumArgs" $ do
            (toXExpr $ Lst [ toXExpr $ SFrm IsEq, toXExpr $ Sym "x"])
            `failsWith`
            NumArgs
          it "(eq? x y z) fails with NumArgs" $ do
            (toXExpr $ Lst [ toXExpr $ SFrm IsEq
                          , toXExpr $ Sym "x"
                          , toXExpr $ Sym "y"
                          , toXExpr $ Sym "z"
                          ])
            `failsWith`
            NumArgs

        describe "lambda" $ do
          it "(lambda (x y) x) returns a function" $ do
            (toXExpr $ Lst [ toXExpr $ SFrm Lambda
                          , toXExpr $ Lst [ toXExpr $ Sym "x"
                                         , toXExpr $ Sym "y"
                                         ]
                          , toXExpr $ Sym "x"
                          ])
            `evaluatesTo`
            (toXExpr $ Fn defaultEnv [ toXExpr $ Sym "x", toXExpr $ Sym "y"] (toXExpr $ Sym "x"))
          it "(lambda (x y)) fails with NumArgs" $ do
            (toXExpr $ Lst [ toXExpr $ SFrm Lambda
                          , toXExpr $ Lst [ toXExpr $ Sym "x"
                                         , toXExpr $ Sym "y"
                                         ]
                          ])
            `failsWith`
            NumArgs
          it "(lambda (x y) z a) fails with NumArgs" $ do
            (toXExpr $ Lst [ toXExpr $ SFrm Lambda
                          , toXExpr $ Lst [ toXExpr $ Sym "x"
                                         , toXExpr $ Sym "y"
                                         ]
                          , toXExpr $ Sym "z"
                          , toXExpr $ Sym "a"
                          ])
            `failsWith`
            NumArgs
          it "(lambda x y) fails with WrongTipe" $ do
            (toXExpr $ Lst [ toXExpr $ SFrm Lambda
                          , toXExpr $ Sym "x"
                          , toXExpr $ Sym "y"
                          ])
            `failsWith`
            WrongTipe

        describe "quote" $ do
          it "(quote <lisp expression>) evaluates to <lisp expression>" $ do
            property prop_eval_quote
          it "(quote x) evaluates to the symbol x" $ do
            (toXExpr $ Lst [ toXExpr $ SFrm Quote
                          , toXExpr $ Sym "x"
                          ])
            `evaluatesTo`
            (toXExpr $ Sym "x")
          it "(quote x y) fails with NumArgs" $ do
            (toXExpr $ Lst [ toXExpr $ SFrm Quote
                          , toXExpr $ Sym "x"
                          , toXExpr $ Sym "y"
                          ])
            `failsWith`
            NumArgs

      describe "function application" $ do
          it "((lambda (x) x) (quote y)) evaluates to the symbol y" $ do
            (toXExpr $ Lst [ toXExpr $ Lst [ toXExpr $ SFrm Lambda
                                         , toXExpr $ Lst [ toXExpr $ Sym "x" ]
                                         , toXExpr $ Sym "x"
                                         ]
                 , toXExpr $ Lst [ toXExpr $ SFrm Quote
                                , toXExpr $ Sym "y"
                                ]
                          ])
            `evaluatesTo`
            (toXExpr $ Sym "y")
          it "(define x (quote y) (y (quote z)) fails with NotFn" $ do
            (toXExpr $ Lst [ toXExpr $ Lst [ toXExpr $ SFrm Quote
                                         , toXExpr $ Sym "x"
                                         ]
                          , toXExpr $ Lst [ toXExpr $ SFrm Quote
                                         , toXExpr $ Sym "y"
                                         ]
                          ])
            `failsWith`
            NotFn

      describe "variable lookup" $ do
          it "evaluating x returns the value bound to the symbol x" $ do
            inEnvEvaluatesTo
              (M.fromList [("foo", toXExpr $ Sym "x")] <> defaultEnv)
              (toXExpr $ Sym "foo")
              (toXExpr $ Sym "x")
          it "evaluating x throws an unknown variable exception when no value is bound to x" $ do
            (toXExpr $ Sym "x") `failsWith` (UnknownVar "x")

inEnvEvaluatesTo :: Env -> XExpr -> XExpr -> Expectation
inEnvEvaluatesTo env expr expected = do
  (result, _) <- Eval.run env expr
  case result of
    (Right actual) -> actual `shouldBe` expected
    (Left err)     -> assertFailure (show err)

evaluatesTo :: XExpr -> XExpr -> Expectation
evaluatesTo expr expected = do
  (result, _) <- Eval.run defaultEnv expr
  case result of
    (Right actual) -> actual `shouldBe` expected
    (Left err)     -> assertFailure (show err)

failsWith :: XExpr -> ErrType -> Expectation
failsWith expr expected = do
  (result, _) <- Eval.run defaultEnv expr
  case result of
    (Right x)     -> assertFailure $ "expected evaluation to fail but received: " ++ (show x)
    (Left (EvalErr actual _)) -> actual `shouldBe` expected

insertsInEnv :: XExpr -> (String, XExpr) -> Expectation
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
  where quotedExpr = toXExpr $ Lst [ toXExpr $ SFrm Quote, expr ]

toXExpr :: Expr -> XExpr
toXExpr expr = XExpr expr dummyInfo

dummyInfo :: Info
dummyInfo = Info (0, 0)

newtype ArbExpr = ArbExpr XExpr deriving (Eq, Show)

instance Arbitrary ArbExpr where
  arbitrary = ArbExpr <$> oneof [ genSym, genSFrm, genLst ]

genSym :: Gen XExpr
genSym = do
  name <- listOf $ oneof [ numeric, lowerAlpha, upperAlpha ]
  return $ toXExpr (Sym name)

genSFrm :: Gen XExpr
genSFrm = (toXExpr . SFrm) <$> elements [ First , Rest , Cons , If , Def , IsAtm , IsEq , Lambda , Quote ]

genLst :: Gen XExpr
genLst = do
  sexprs <- listOf $ oneof [ genSym, genSFrm, resize 2 genLst ]
  return $ toXExpr (Lst sexprs)
