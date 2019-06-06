module Parser where

type Ident = String

data Expr
  = Integer Int
  | Lambda Ident Expr
  | Apply Expr Expr
  | Variable Ident
  deriving Show
