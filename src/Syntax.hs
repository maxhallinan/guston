module Syntax (Env, Sexpr(..), SpecialForm(..), defaultEnv) where

import qualified Data.Map as M

data Sexpr =
    Sym String
  | SFrm SpecialForm
  | Fn Env [Sexpr] Sexpr
  | Lst [Sexpr]
  deriving (Eq, Show)

type Env = M.Map String Sexpr

defaultEnv :: Env
defaultEnv = M.empty

data SpecialForm =
    Begin
  | Car
  | Cdr
  | Cons
  | Cond
  | Def
  | IsAtm
  | IsEq
  | Lambda
  | Quote
  deriving (Eq, Show)
