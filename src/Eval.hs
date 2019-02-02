{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Eval (eval, runEval) where

import Control.Monad.IO.Class
import qualified Control.Monad.State as S
import qualified Data.Map as M

import Syntax (Env, Sexpr(..))

newtype Eval a = Eval { runEval :: S.StateT Env IO a }
  deriving ( Applicative
           , Functor
           , Monad
           , S.MonadState Env
           , MonadIO
           )

data EvalErr =
    Unknown
  | Debug (Either EvalErr Sexpr)
  deriving (Eq, Show)

eval :: Sexpr -> Eval (Either EvalErr Sexpr)
eval sexpr = do
  case sexpr of
    (Sym key) -> do
      env <- S.get
      case M.lookup key env of
        Just val  -> return $ Right $ val
        Nothing   -> return $ Left Unknown

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

    (Lst [Sym "lambda", Lst params, body]) -> do
      env <- S.get
      return $ Right $ Lambda env params body

    (Lst [Sym "quote", x])  -> return $ Right x
    (Lst (Sym "quote": _))  -> return $ Left Unknown

    (Lst (x:xs)) -> do
      return $ Left Unknown

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
