{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Eval (EvalErr(..), eval, evalFile, run, runFile, runEval) where

import Data.Traversable (sequence)
import qualified Control.Applicative as A
import Control.Monad.Except as E
import qualified Control.Monad.State as S
import qualified Data.Map as M

import qualified Syntax as Sx
import Syntax (Env, Expr(..), Info, Sexpr(..), SpecialForm(..))

newtype Eval a = Eval { runEval :: E.ExceptT EvalErr (S.StateT Env IO) a }
  deriving ( Applicative
           , Functor
           , Monad
           , S.MonadState Env
           , MonadIO
           , E.MonadError EvalErr
           )

run :: Env -> Expr -> IO (Either EvalErr Expr, Env)
run env expr = rn expr env
  where rn = S.runStateT . E.runExceptT . runEval . eval

runFile :: Env -> [Expr] -> IO (Either EvalErr [Expr], Env)
runFile env exprs = rn evaled env
  where evaled  = sequence (eval <$> exprs)
        rn      = S.runStateT . E.runExceptT . runEval

data EvalErr
  = NumArgs
  | WrongTipe
  | LstLength
  | UnknownVar String
  | NotFn
  | Unknown
  | NotPair
  deriving (Eq, Show)

evalFile :: [Expr] -> Eval [Expr]
evalFile xs = sequence $ eval <$> xs

eval :: Expr -> Eval Expr
eval (Expr sexpr info) = do
  case sexpr of
    (Sym name) -> evalSym name
    (Lst ((Expr (SFrm sfrm) _):args)) -> evalSFrm info sfrm args
    (Lst xs) -> evalLst info xs
    _ -> E.throwError Unknown

evalSym :: String -> Eval Expr
evalSym name = do
  env <- S.get
  case M.lookup name env of
    Just expr   -> return expr
    Nothing     -> E.throwError $ UnknownVar name

evalSFrm :: Info -> SpecialForm -> [Expr] -> Eval Expr
evalSFrm _ _ [] = E.throwError NumArgs
evalSFrm info sfrm args = do
  case sfrm of
    Car     -> evalCar info args
    Cdr     -> evalCdr info args
    Cond    -> evalCond info args
    Cons    -> evalCons info args
    Def     -> evalDef info args
    IsAtm   -> evalIsAtm info args
    IsEq    -> evalIsEq info args
    Lambda  -> evalLambda info args
    Quote   -> evalQuote info args

evalIsAtm :: Info -> [Expr] -> Eval Expr
evalIsAtm info [x]   = do
  y <- eval x
  case y of
    Expr (Sym _) _  -> return $ Expr (Sym "t") info
    Expr (Lst []) _ -> return $ Expr (Sym "t") info
    Expr _ _        -> return $ Expr (Lst []) info
evalIsAtm _ _ = E.throwError NumArgs

evalIsEq :: Info -> [Expr] -> Eval Expr
evalIsEq info [x, y] = do
  (Expr x2 _, Expr y2 _) <- A.liftA2 (,) (eval x) (eval y)
  case (x2, y2) of
    (Sym name1, Sym name2) ->
      if name1 == name2
      then return $ Expr (Sym "t") info
      else return $ Expr (Lst []) info
    (Lst [], Lst []) -> return $ Expr (Sym "t") info
    (_, _)  -> return $ Expr (Lst []) info
evalIsEq _ _ = E.throwError NumArgs

evalCar :: Info -> [Expr] -> Eval Expr
evalCar info [x] = do
  Expr e _ <- eval x
  case e of
    (Lst [])      -> E.throwError LstLength
    (Lst (y:_))   -> return $ Expr (Sx.sexpr y) info
    _             -> E.throwError WrongTipe
evalCar _ _ = E.throwError NumArgs

evalCdr :: Info -> [Expr] -> Eval Expr
evalCdr info [x] = do
  Expr e _ <- eval x
  case e of
    (Lst [])      -> return $ Expr (Lst []) info
    (Lst (_:ys))  -> return $ Expr (Lst ys) info
    _             -> E.throwError WrongTipe
evalCdr _ _ = E.throwError NumArgs

evalCons :: Info -> [Expr] -> Eval Expr
evalCons info [x, xs] = do
  Expr e1 i1 <- eval x
  Expr e2 i2 <- eval xs
  case (e1, e2) of
    (y, (Lst ys)) -> return $ Expr (Lst ((Expr e1 i1):ys)) info
    (_, _)        -> E.throwError WrongTipe
    _             -> E.throwError Unknown
evalCons _ _ = E.throwError NumArgs

evalCond :: Info -> [Expr] -> Eval Expr
evalCond _ [] = E.throwError NumArgs
evalCond info (c:cs) =
  case Sx.sexpr c of
    Lst [] -> E.throwError NotPair
    Lst [p, e] -> do
      Expr x _ <- eval p
      case x of
        (Sym "t") -> do
          Expr x' _ <- eval e
          return $ Expr x' info
        (Lst [])  -> do
          Expr x' _ <- evalCond info cs
          return $ Expr x' info
        _         -> E.throwError WrongTipe
    Lst (_:_) -> E.throwError NotPair
    _         -> E.throwError WrongTipe

evalDef :: Info -> [Expr] -> Eval Expr
evalDef info [Expr (Sym key) _, expr] = do
  (Expr val i) <- eval expr
  env <- S.get
  _   <- S.put (M.insert key (Expr val i) env)
  return $ Expr val info
evalDef _ [_,_] = E.throwError WrongTipe
evalDef _ _ = E.throwError NumArgs

evalLambda :: Info -> [Expr] -> Eval Expr
evalLambda info [Expr (Lst params) _, body] = do
  env <- S.get
  return $ Expr (Fn env params body) info
evalLambda _ [_,_] = E.throwError WrongTipe
evalLambda _ _ = E.throwError NumArgs

evalQuote :: Info -> [Expr] -> Eval Expr
evalQuote info [Expr x _] = return $ Expr x info
evalQuote _ _ = E.throwError NumArgs

evalLst :: Info -> [Expr] -> Eval Expr
evalLst _ [] = E.throwError NumArgs
evalLst info (x:xs) = do
  Expr fn _ <- eval x
  case fn of
    Fn localEnv params body -> do
      globalEnv <- S.get
      let env = localEnv <> globalEnv
      Eval $ E.mapExceptT (S.withStateT (const env)) (runEval $ applyLambda info params xs body)
    _  -> E.throwError NotFn

applyLambda :: Info -> [Expr] -> [Expr] -> Expr -> Eval Expr
applyLambda info params args body = do
  env     <- S.get
  args'   <- traverse eval args
  let env' = M.fromList (zipWith (\(Expr (Sym k) _) v -> (k, v)) params args') <> env
  Eval $ E.mapExceptT (S.withStateT (const env')) (runEval $ eval body)
