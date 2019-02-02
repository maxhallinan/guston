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
  deriving (Eq, Show)

defaultEnv :: Env
defaultEnv = M.empty

eval :: Sexpr -> Eval (Either EvalErr Sexpr)
eval sexpr = do
  case sexpr of
    (Lst [Sym "atom?", Sym _])  -> return $ Right $ Sym "true"
    (Lst [Sym "atom?", _])      -> return $ Right $ Sym "false"
    _ -> return $ Left Unknown
