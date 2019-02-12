module Syntax
  ( Env
  , Expr(..)
  , Info(..)
  , SpecialForm(..)
  , XExpr(..)
  , defaultEnv
  ) where

import qualified Data.Map as M
import Data.String (unwords)

data XExpr = XExpr Expr Info

instance Eq XExpr where
  (==) (XExpr e1 _) (XExpr e2 _) = e1 == e2

instance Show XExpr where
  show (XExpr s _) = show s

data Info =
  Info { infoOffsetRange :: (Int, Int)
       }

instance Show Info where
  show (Info (start, end)) = (show start) ++ " - " ++ (show end)

instance Eq Info where
  (==) (Info (s1, e1)) (Info (s2, e2)) = (s1 == s2) && (e1 == e2)

data Expr =
    Sym String
  | SFrm SpecialForm
  | Fn Env [XExpr] XExpr
  | Lst [XExpr]
  deriving (Eq)

instance Show Expr where
  show s =
    case s of
      Sym name    -> name
      SFrm sfrm   -> show sfrm
      Fn _ _ _    -> "<function>"
      Lst sexprs  -> concat [ "(", unwords $ show <$> sexprs, ")" ]

data SpecialForm =
    First
  | Rest
  | Cons
  | If
  | Def
  | IsAtm
  | IsEq
  | Lambda
  | Quote
  deriving (Eq)

instance Show SpecialForm where
  show sfrm = case sfrm of
    First   -> "first"
    Rest    -> "rest"
    Cons    -> "::"
    If      -> "if"
    Def     -> "="
    IsAtm   -> "atom?"
    IsEq    -> "=="
    Lambda  -> "fn"
    Quote   -> "quote"

type Env = M.Map String XExpr

defaultEnv :: Env
defaultEnv = M.empty
