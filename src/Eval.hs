{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Eval (EvalErr(..), eval, evalFile, runEval) where

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
  | UnknownVar String
  | NotFn
  | Unknown
  | NotPair
  deriving (Eq, Show)

evalFile :: [Sexpr] -> Eval [Either EvalErr Sexpr]
evalFile xs = sequence $ eval <$> xs

eval :: Sexpr -> Eval (Either EvalErr Sexpr)
eval sexpr = do
  case sexpr of
    (Sym name)             -> evalSym name
    (Lst (SFrm sfrm:args)) -> evalSFrm sfrm args
    (Lst xs)               -> evalLst xs
    _                      -> return $ Left $ Unknown

evalSym :: String -> Eval (Either EvalErr Sexpr)
evalSym name = do
  env <- S.get
  case M.lookup name env of
    Just val  -> return $ Right $ val
    Nothing   -> return $ Left $ UnknownVar name

evalSFrm :: SpecialForm -> [Sexpr] -> Eval (Either EvalErr Sexpr)
evalSFrm _ [] = return $ Left NumArgs
evalSFrm sfrm args = do
  case sfrm of
    Car     -> evalCar args
    Cdr     -> evalCdr args
    Cond    -> evalCond args
    Cons    -> evalCons args
    Def     -> evalDef args
    IsAtm   -> evalIsAtm args
    IsEq    -> evalIsEq args
    Lambda  -> evalLambda args
    Quote   -> evalQuote args

evalIsAtm :: [Sexpr] -> Eval (Either EvalErr Sexpr)
evalIsAtm [Sym _]   = return $ Right $ Sym "true"
evalIsAtm [Lst []]  = return $ Right $ Sym "true"
evalIsAtm [Lst _]   = return $ Right $ Sym "false"
evalIsAtm _         = return $ Left NumArgs

evalIsEq :: [Sexpr] -> Eval (Either EvalErr Sexpr)
evalIsEq [Sym x, Sym y] =
  if x == y
  then return $ Right $ Sym "true"
  else return $ Right $ Sym "false"
evalIsEq [Lst [], Lst []] = return $ Right $ Sym "true"
evalIsEq [_,_] = return $ Right $ Sym "false"
evalIsEq _     = return $ Left NumArgs

evalCar :: [Sexpr] -> Eval (Either EvalErr Sexpr)
evalCar [x] = do
  e <- eval x
  case e of
    Right (Lst [])      -> return $ Left $ LstLength
    Right (Lst (y:_))   -> return $ Right $ y
    Right _             -> return $ Left WrongTipe
    Left _              -> return e
evalCar _ = return $ Left NumArgs

evalCdr :: [Sexpr] -> Eval (Either EvalErr Sexpr)
evalCdr [x] = do
  e <- eval x
  case e of
    Right (Lst [])      -> return $ Right $ Lst []
    Right (Lst (_:ys))  -> return $ Right $ Lst ys
    Right _             -> return $ Left WrongTipe
    Left _              -> return e
evalCdr _ = return $ Left NumArgs

evalCons :: [Sexpr] -> Eval (Either EvalErr Sexpr)
evalCons [x, xs]  = do
  e1 <- eval x
  e2 <- eval xs
  case (e1, e2) of
    (Right y, Right (Lst ys)) -> return $ Right $ Lst (y:ys)
    (Right _, Right _)        -> return $ Left WrongTipe
    _                         -> return $ Left $ Unknown
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
evalDef [Sym key, expr] = do
  val <- eval expr
  case val of
    (Right x) -> do
      env <- S.get
      _   <- S.put (M.insert key x env)
      return val
    _ -> return val
evalDef [_,_]   = return $ Left WrongTipe
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
    (Right (Fn closure params body), Right args') -> do
      let env' = M.fromList (zipWith (\(Sym k) v -> (k, v)) params args') <> closure
      Eval $ S.withStateT (const env') (runEval . eval $ body)
    (Right _, _)  -> return $ Left NotFn
    (Left err, _) -> return $ Left err
