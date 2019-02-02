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
    Car
  | Cdr
  | Cns
  | Cond
  | Def
  | IsAtm
  | IsEq
  | Lambda
  | Quot
  deriving (Eq, Show)
