module Desugar where

import qualified Operators as Op
import qualified Parse     as P

data Expr
  = Integer Int
  | Lambda String Expr
  | Apply Expr Expr
  | Variable String
  | BinaryOp Op.BinaryOp Expr Expr
  | SingleOp Op.SingleOp Expr
  | Tuple [Expr]
  | LetIn [Def] Expr
  deriving (Show, Eq)

data DefBody
  = Name Expr
  deriving (Show, Eq)

data Def = Def String DefBody deriving (Show, Eq)

type Code = [Def]

desugarLambda :: [String] -> Expr -> Expr
desugarLambda = flip $ foldr Lambda

desugarExpr :: P.Expr -> Expr
desugarExpr (P.Lambda ps body)  = desugarLambda ps $ desugarExpr body
desugarExpr (P.Integer i)       = Integer i
desugarExpr (P.Apply a b)       = Apply (desugarExpr a) (desugarExpr b)
desugarExpr (P.Variable x)      = Variable x
desugarExpr (P.BinaryOp op a b) = BinaryOp op (desugarExpr a) (desugarExpr b)
desugarExpr (P.SingleOp op x)   = SingleOp op $ desugarExpr x
desugarExpr (P.Tuple xs)        = Tuple $ map desugarExpr xs
desugarExpr (P.LetIn defs x)    = LetIn (map desugarDef defs) $ desugarExpr x

desugarDefBody :: P.DefBody -> DefBody
desugarDefBody (P.Name ps body []) = Name $ desugarLambda ps $ desugarExpr body
desugarDefBody (P.Name ps body defs) = Name $ LetIn (map desugarDef defs) $ desugarLambda ps $ desugarExpr body

desugarDef :: P.Def -> Def
desugarDef (P.Def name body) = Def name $ desugarDefBody body

desugar :: P.Code -> Code
desugar = map desugarDef
