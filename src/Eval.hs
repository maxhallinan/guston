{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Eval (defaultEnv, eval, runEval) where

import qualified Control.Monad.State as S
import qualified Data.Map as M

import Syntax (Sexpr(..))

type Env = M.Map String Sexpr

newtype Eval a = Eval { runEval :: S.StateT Env IO a }
  deriving ( Applicative
           , Functor
           , Monad
           , S.MonadState Env
           )

data EvalErr =
    Unknown
  | Debug (Either EvalErr Sexpr, Either EvalErr Sexpr)
  deriving (Eq, Show)

defaultEnv :: Env
defaultEnv = M.empty

eval :: Sexpr -> Eval (Either EvalErr Sexpr)
eval sexpr = do
  case sexpr of
    (Lst [Sym "atom?", Sym _])  -> return $ Right $ Sym "true"
    (Lst [Sym "atom?", _])      -> return $ Right $ Sym "false"
    (Lst (Sym "atom?": _))      -> return $ Left Unknown

    (Lst [Sym "car", Lst []])     -> return $ Left Unknown
    (Lst [Sym "car", Lst (x:_)])  -> return $ Right x
    (Lst (Sym "car": _))          -> return $ Left Unknown

    (Lst [Sym "cdr", Lst []])     -> return $ Right $ Lst []
    (Lst [Sym "cdr", Lst (_:xs)]) -> return $ Right $ Lst xs
    (Lst (Sym "cdr": _))          -> return $ Left Unknown

    (Lst [Sym "cons", x, Lst xs]) -> do 
      e1 <- eval x 
      e2 <- eval $ Lst xs
      case (e1, e2) of 
        (Right y, Right (Lst ys)) -> return $ Right $ Lst (y:ys)
        (Right _, Right _)        -> return $ Left $ Unknown
        _                         -> return $ Left $ Unknown
    (Lst (Sym "cons": _)) -> return $ Left $ Unknown

    (Lst [Sym "cond", Lst conds]) -> evalCond conds
    (Lst (Sym "cond": _)) -> return $ Left $ Unknown

    (Lst [Sym "define", Sym key, expr, body]) -> do 
      val <- eval expr
      case val of
        (Right x) -> do
          env <- S.get
          _   <- S.put (M.insert key x env)
          eval body

        _ -> return val 

    (Lst [Sym "eq?", Sym x, Sym y]) -> 
      if x == y 
      then return $ Right $ Sym "true" 
      else return $ Right $ Sym "false"
    (Lst (Sym "eq?": _)) -> return $ Left Unknown

    (Lst [Sym "quote", x])  -> return $ Right x
    (Lst (Sym "quote": _))  -> return $ Left Unknown 

    _ -> return $ Left Unknown

evalCond :: [Sexpr] -> Eval (Either EvalErr Sexpr)
evalCond [] = return $ Right $ Sym "false"
evalCond (Lst [predicate, body]: cs) = do
  x <- eval predicate
  case x of
    Right (Sym "true")  -> eval body
    Right (Sym "false") -> evalCond cs
    _                   -> return $ Left Unknown
evalCond _ = return $ Left Unknown    
