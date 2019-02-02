module Syntax (Env, Sexpr(..), defaultEnv) where

import qualified Data.Map as M

data Sexpr = 
    Sym String
  | Lamd Env [Sexpr] Sexpr
  | Lst [Sexpr]
  deriving (Eq, Show)

type Env = M.Map String Sexpr

defaultEnv :: Env
defaultEnv = M.empty
