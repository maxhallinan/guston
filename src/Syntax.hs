module Syntax (Env, Sexpr(..), defaultEnv) where

import qualified Data.Map as M

data Sexpr = 
    Sym String
  | Lambda Env [Sexpr] Sexpr
  | Lst [Sexpr]
  deriving (Eq, Show)

type Env = M.Map String Sexpr

defaultEnv :: Env
defaultEnv = M.empty
