module Syntax
  ( Env
  , Expr(..)
  , Sexpr(..)
  , SpecialForm(..)
  , Info(..)
  , defaultEnv
  ) where

import qualified Data.Map as M
import Data.String (unwords)

data Expr =
  Expr { sexpr :: Sexpr
       , info :: Info
       }
       deriving (Eq)

instance Show Expr where
  show (Expr s _) = show s

data Info =
  Info { infoCol :: Int
       , infoLine :: Int
       , infoSourceName :: FilePath
       }

instance Show Info where
  show (Info col line sourceName) =
    sourceName ++ " " ++ (show line) ++ ":" ++ (show col)

instance Eq Info where
  (==) (Info c1 l1 s1) (Info c2 l2 s2) = (c1 == c2) && (l1 == l2) && (s1 == s2)

data Sexpr =
    Sym String
  | SFrm SpecialForm
  | Fn Env [Expr] Expr
  | Lst [Expr]
  deriving (Eq)

instance Show Sexpr where
  show s =
    case s of
      Sym name    -> name
      SFrm sfrm   -> show sfrm
      Fn _ _ _    -> "<lambda function>"
      Lst sexprs  -> concat [ "(", unwords $ show <$> sexprs, ")" ]

type Env = M.Map String Expr

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
