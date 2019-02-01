{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Eval (defaultEnv, evalInEnv) where

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
  deriving (Eq, Show)

defaultEnv :: Env
defaultEnv = M.empty

evalInEnv :: Env -> Sexpr -> IO (Either EvalErr Sexpr, Env)
evalInEnv env sexpr = S.runStateT evaled env
  where evaled = runEval . eval $ sexpr

eval :: Sexpr -> Eval (Either EvalErr Sexpr)
eval sexpr = return (Right sexpr)
