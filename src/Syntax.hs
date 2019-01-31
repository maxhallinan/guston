module Syntax (Sexpr(..)) where

data Sexpr = 
    Symbol String
  | List [Sexpr]
  deriving (Eq, Show)
