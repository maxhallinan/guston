{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Eval (defaultEnv, eval, runEval) where

import qualified Control.Applicative (empty)
import qualified Control.Monad.State as S
import qualified Data.Map as M

import Syntax (Sexpr(..))

type Env = M.Map String Sexpr

newtype Eval a = Eval { runEval :: S.StateT Env IO a }
  deriving ( Applicative
           , Functor
           , Monad
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
        (Right y, Right _)        -> return $ Left $ Unknown
        _                         -> return $ Left $ Unknown
    (Lst (Sym "cons": _)) -> return $ Left $ Unknown

    (Lst [Sym "eq?", Sym x, Sym y]) -> 
      if x == y 
      then return $ Right $ Sym "true" 
      else return $ Right $ Sym "false"
    (Lst (Sym "eq?": _)) -> return $ Left Unknown

    (Lst [Sym "quote", x])  -> return $ Right x
    (Lst (Sym "quote": _))  -> return $ Left Unknown 

    _ -> return $ Left Unknown
