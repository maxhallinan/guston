{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Eval (EvalErr(..), eval, evalFile, run, runFile, runEval) where

import Data.Traversable (sequence)
import Control.Monad.Except as E
import qualified Control.Monad.State as S
import qualified Data.Map as M

import Syntax (Env, Sexpr(..), SpecialForm(..))

newtype Eval a = Eval { runEval :: E.ExceptT EvalErr (S.StateT Env IO) a }
  deriving ( Applicative
           , Functor
           , Monad
           , S.MonadState Env
           , MonadIO
           , E.MonadError EvalErr
           )

run :: Env -> Sexpr -> IO (Either EvalErr Sexpr, Env)
run env sexpr = rn sexpr env
  where rn = S.runStateT . E.runExceptT . runEval . eval

runFile :: Env -> [Sexpr] -> IO (Either EvalErr [Sexpr], Env)
runFile env sexprs = rn evaled env
  where evaled  = sequence (eval <$> sexprs)
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

evalFile :: [Sexpr] -> Eval [Sexpr]
evalFile xs = sequence $ eval <$> xs

eval :: Sexpr -> Eval Sexpr
eval sexpr = do
  case sexpr of
    (Sym name)             -> evalSym name
    (Lst (SFrm sfrm:args)) -> evalSFrm sfrm args
    (Lst xs)               -> evalLst xs
    _                      -> E.throwError Unknown

evalSym :: String -> Eval Sexpr
evalSym name = do
  env <- S.get
  case M.lookup name env of
    Just val  -> return val
    Nothing   -> E.throwError $ UnknownVar name

evalSFrm :: SpecialForm -> [Sexpr] -> Eval Sexpr
evalSFrm _ [] = E.throwError NumArgs
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

evalIsAtm :: [Sexpr] -> Eval Sexpr
evalIsAtm [x]   = do
  y <- eval x
  case y of
    (Sym _)   -> return $ Sym "t"
    (Lst [])  -> return $ Sym "t"
    _         -> return $ Lst []
evalIsAtm _ = E.throwError NumArgs

evalIsEq :: [Sexpr] -> Eval Sexpr
evalIsEq [x, y] = do
  x' <- eval x
  y' <- eval y
  case (x', y') of
    (Sym name1, Sym name2) ->
      if name1 == name2
      then return $ Sym "t"
      else return $ Lst []
    (Lst [], Lst [])    -> return $ Sym "t"
    (_, _)  -> return $ Lst []
evalIsEq _ = E.throwError NumArgs

evalCar :: [Sexpr] -> Eval Sexpr
evalCar [x] = do
  e <- eval x
  case e of
    (Lst [])      -> E.throwError LstLength
    (Lst (y:_))   -> return $ y
    _             -> E.throwError WrongTipe
evalCar _ = E.throwError NumArgs

evalCdr :: [Sexpr] -> Eval Sexpr
evalCdr [x] = do
  e <- eval x
  case e of
    (Lst [])      -> return $ Lst []
    (Lst (_:ys))  -> return $ Lst ys
    _             -> E.throwError WrongTipe
evalCdr _ = E.throwError NumArgs

evalCons :: [Sexpr] -> Eval Sexpr
evalCons [x, xs] = do
  e1 <- eval x
  e2 <- eval xs
  case (e1, e2) of
    (y, (Lst ys)) -> return $ Lst (y:ys)
    (_, _)        -> E.throwError WrongTipe
    _             -> E.throwError Unknown
evalCons _ = E.throwError NumArgs

evalCond :: [Sexpr] -> Eval Sexpr
evalCond [] = E.throwError NumArgs
evalCond (Lst [p, e]:cs) = do
  x <- eval p
  case x of
    (Sym "t") -> eval e
    (Lst [])  -> evalCond cs
    _         -> E.throwError WrongTipe
evalCond (Lst _:_)  = E.throwError NotPair
evalCond (_:_)      = E.throwError WrongTipe

evalDef :: [Sexpr] -> Eval Sexpr
evalDef [Sym key, expr] = do
  val <- eval expr
  env <- S.get
  _   <- S.put (M.insert key val env)
  return val
evalDef [_,_] = E.throwError WrongTipe
evalDef _     = E.throwError NumArgs

evalLambda :: [Sexpr] -> Eval Sexpr
evalLambda [Lst params, body] = do
  env <- S.get
  return $ Fn env params body
evalLambda [_,_] = E.throwError WrongTipe
evalLambda _     = E.throwError NumArgs

evalQuote :: [Sexpr] -> Eval Sexpr
evalQuote [x] = return x
evalQuote _   = E.throwError NumArgs

evalLst :: [Sexpr] -> Eval Sexpr
evalLst [] = E.throwError NumArgs
evalLst (x:xs) = do
  fn   <- eval x
  case fn of
    Fn localEnv params body -> do
      globalEnv <- S.get
      let env = localEnv <> globalEnv
      Eval $ E.mapExceptT (S.withStateT (const env)) (runEval $ applyLambda params xs body)
    _  -> E.throwError NotFn

applyLambda :: [Sexpr] -> [Sexpr] -> Sexpr -> Eval Sexpr
applyLambda params args body = do
  env     <- S.get
  args'   <- traverse eval args
  let env' = M.fromList (zipWith (\(Sym k) v -> (k, v)) params args') <> env
  Eval $ E.mapExceptT (S.withStateT (const env')) (runEval $ eval body)
