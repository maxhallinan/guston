module Syntax (Sexpr(..)) where

data Sexpr = 
    Sym String
  | List [Sexpr]
  deriving (Eq, Show)
