{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Eval (EvalErr(..), eval, runEval) where

import Control.Monad.IO.Class
import qualified Control.Monad.State as S
import qualified Data.Map as M

import Syntax (Env, Sexpr(..), SpecialForm(..))

newtype Eval a = Eval { runEval :: S.StateT Env IO a }
  deriving ( Applicative
           , Functor
           , Monad
           , S.MonadState Env
           , MonadIO
           )

data EvalErr
  = NumArgs
  | WrongTipe
  | LstLength
  | UnknownVar
  | NotFn
  | Unknown
  | NotPair
  deriving (Eq, Show)

eval :: Sexpr -> Eval (Either EvalErr Sexpr)
eval sexpr = do
  case sexpr of
    (Sym name)             -> evalSym name
    (Lst (SFrm sfrm:args)) -> evalSFrm sfrm args
    (Lst xs)               -> evalLst xs
    _                      -> return $ Left Unknown

evalSym :: String -> Eval (Either EvalErr Sexpr)
evalSym name = do
  env <- S.get
  case M.lookup name env of
    Just val  -> return $ Right $ val
    Nothing   -> return $ Left UnknownVar

evalSFrm :: SpecialForm -> [Sexpr] -> Eval (Either EvalErr Sexpr)
evalSFrm _ [] = return $ Left NumArgs
evalSFrm sfrm args = do
  case sfrm of
    Begin   -> evalBegin args
    Car     -> evalCar args
    Cdr     -> evalCdr args
    Cond    -> evalCond args
    Cons    -> evalCons args
    Def     -> evalDef args
    IsAtm   -> evalIsAtm args
    IsEq    -> evalIsEq args
    Lambda  -> evalLambda args
    Quote   -> evalQuote args

evalBegin :: [Sexpr] -> Eval (Either EvalErr Sexpr)
evalBegin []      = return $ Left NumArgs
evalBegin (x:xs)  = case xs of
    []  -> eval x
    _   -> do
      _ <- eval x
      evalBegin xs

evalIsAtm :: [Sexpr] -> Eval (Either EvalErr Sexpr)
evalIsAtm [Sym _] = return $ Right $ Sym "true"
evalIsAtm [_]     = return $ Right $ Sym "false"
evalIsAtm _       = return $ Left NumArgs

evalIsEq :: [Sexpr] -> Eval (Either EvalErr Sexpr)
evalIsEq [Sym x, Sym y] =
  if x == y
  then return $ Right $ Sym "true"
  else return $ Right $ Sym "false"
evalIsEq [_,_] = return $ Left WrongTipe
evalIsEq _     = return $ Left NumArgs

evalCar :: [Sexpr] -> Eval (Either EvalErr Sexpr)
evalCar [Lst []]    = return $ Left LstLength
evalCar [Lst (x:_)] = return $ Right x
evalCar _           = return $ Left WrongTipe

evalCdr :: [Sexpr] -> Eval (Either EvalErr Sexpr)
evalCdr [Lst []]      = return $ Left $ LstLength
evalCdr [Lst (_:xs)]  = return $ Right $ Lst xs
evalCdr _             = return $ Left WrongTipe

evalCons :: [Sexpr] -> Eval (Either EvalErr Sexpr)
evalCons [x, Lst xs]  = do
  e1 <- eval x
  e2 <- eval $ Lst xs
  case (e1, e2) of
    (Right y, Right (Lst ys)) -> return $ Right $ Lst (y:ys)
    (Right _, Right _)        -> return $ Left WrongTipe
    _                         -> return $ Left Unknown
evalCons [_, _] = return $ Left WrongTipe
evalCons _      = return $ Left NumArgs

evalCond :: [Sexpr] -> Eval (Either EvalErr Sexpr)
evalCond [] = return $ Left NumArgs
evalCond (Lst [p, e]:cs) = do
  x <- eval p
  case x of
    Right (Sym "true")  -> eval e
    Right (Sym "false") -> evalCond cs
    Right _             -> return $ Left WrongTipe
    Left err            -> return $ Left $ err
evalCond (Lst _:_)  = return $ Left NotPair
evalCond (_:_)      = return $ Left WrongTipe

evalDef :: [Sexpr] -> Eval (Either EvalErr Sexpr)
evalDef [Sym key, expr, body] = do
  val <- eval expr
  case val of
    (Right x) -> do
      env <- S.get
      _   <- S.put (M.insert key x env)
      eval body
    _ -> return val
evalDef [_,_,_] = return $ Left WrongTipe
evalDef _       = return $ Left NumArgs

evalLambda :: [Sexpr] -> Eval (Either EvalErr Sexpr)
evalLambda [Lst params, body] = do
  env <- S.get
  return $ Right $ Fn env params body
evalLambda [_,_]    = return $ Left WrongTipe
evalLambda _        = return $ Left NumArgs

evalQuote :: [Sexpr] -> Eval (Either EvalErr Sexpr)
evalQuote [x] = return $ Right x
evalQuote _   = return $ Left NumArgs

evalLst :: [Sexpr] -> Eval (Either EvalErr Sexpr)
evalLst [] = return $ Left NumArgs
evalLst (x:xs) = do
  fn   <- eval x
  args <- traverse eval xs
  case (fn, traverse id args) of
    (Right (Fn env params body), Right args') -> do
      let env' = M.fromList (zipWith (\(Sym k) v -> (k, v)) params args') <> env
      Eval $ S.withStateT (const env') (runEval . eval $ body)
    (Right _, _)  -> return $ Left NotFn
    (Left err, _) -> return $ Left err
