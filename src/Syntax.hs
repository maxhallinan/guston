module Syntax (Env, Sexpr(..), SpecialForm(..), defaultEnv) where

import qualified Data.Map as M
import Data.String (unwords)

data Sexpr =
    Sym String
  | SFrm SpecialForm
  | Fn Env [Sexpr] Sexpr
  | Lst [Sexpr]
  deriving (Eq)

instance Show Sexpr where
  show sexpr = case sexpr of
    Sym name    -> name
    SFrm sfrm   -> show sfrm
    Fn _ _ _    -> "<lambda function>"
    Lst sexprs  -> concat [ "(", unwords $ show <$> sexprs, ")" ]

type Env = M.Map String Sexpr

defaultEnv :: Env
defaultEnv = M.empty

data SpecialForm =
    Car
  | Cdr
  | Cons
  | Cond
  | Def
  | IsAtm
  | IsEq
  | Lambda
  | Quote
  deriving (Eq)

instance Show SpecialForm where
  show sfrm = case sfrm of
    Car     -> "car"
    Cdr     -> "cdr"
    Cons    -> "cons"
    Cond    -> "cond"
    Def     -> "define"
    IsAtm   -> "atom?"
    IsEq    -> "eq?"
    Lambda  -> "lambda"
    Quote   -> "quote"
