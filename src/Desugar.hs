module Desugar where

import qualified Parse as P

data Expr
  = Integer Int
  | Lambda String Expr
  | Apply Expr Expr
  | Variable String
  deriving (Show)

desugar_lambda :: [String] -> Expr -> Expr
desugar_lambda (x:xs) = Lambda x . desugar_lambda xs
desugar_lambda [] = id

desugar :: P.Expr -> Expr
desugar (P.Lambda ps body) = desugar_lambda ps $ desugar body
desugar (P.Integer i) = Integer i
desugar (P.Apply a b) = Apply (desugar a) (desugar b)
desugar (P.Variable x) = Variable x
