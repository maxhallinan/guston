module Syntax
  ( Callframe(..)
  , Callstack
  , Env
  , Expr(..)
  , Sexpr(..)
  , SpecialForm(..)
  , Info(..)
  , callframe
  , defaultEnv
  , emptyCallstack
  , popCallframe
  , pushCallframe
  ) where

import qualified Data.Map as M
import Data.String (unwords)

data Expr = Expr Sexpr Info deriving (Eq)

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

type Callstack = [Callframe]

data Callframe = Callframe Expr deriving (Show)

callframe :: Expr -> Callframe
callframe expr = Callframe expr

emptyCallstack :: Callstack
emptyCallstack = []

pushCallframe :: Callframe -> Callstack -> Callstack
pushCallframe frame stack = (frame:stack)

popCallframe :: Callstack -> Callstack
popCallframe [] = []
popCallframe (_:stack) = stack

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
