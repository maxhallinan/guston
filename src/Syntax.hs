module Syntax (Sexpr(..)) where

data Sexpr = 
    Sym String
  | Lst [Sexpr]
  deriving (Eq, Show)
