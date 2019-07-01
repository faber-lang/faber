module Desugar where

import           Data.Tuple.Extra (second)
import qualified Operators        as Op
import           Parse            (TypeExpr (..), TypeScheme)
import qualified Parse            as P

data Expr
  = Integer Int
  | Lambda String Expr
  | Apply Expr Expr
  | CtorApp String Expr
  | Variable String
  | BinaryOp Op.BinaryOp Expr Expr
  | SingleOp Op.SingleOp Expr
  | Tuple [Expr]
  | LetIn [NameDef] Expr
  | If Expr Expr Expr
  | Match Expr [(P.Pattern, Expr)]
  deriving (Show, Eq)

data NameDef
  = NameDef String Expr
  | TypeAnnot String TypeScheme
  deriving (Show, Eq)

newtype TypeDef
  = Variant [(String, TypeExpr)]
  deriving (Show, Eq)

data Def
  = Name NameDef
  | Type String [String] TypeDef
  deriving (Show, Eq)

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
desugarExpr (P.LetIn defs x)    = LetIn (map desugarNameDef defs) $ desugarExpr x
desugarExpr (P.If c t e)        = If (desugarExpr c) (desugarExpr t) (desugarExpr e)
desugarExpr (P.Match t arms)    = Match (desugarExpr t) (map (second desugarExpr) arms)

desugarNameDef :: P.NameDef -> NameDef
desugarNameDef (P.NameDef name ps body []) = NameDef name $ desugarLambda ps $ desugarExpr body
desugarNameDef (P.NameDef name ps body defs) = NameDef name $ LetIn (map desugarNameDef defs) $ desugarLambda ps $ desugarExpr body
-- using `TypeScheme` from `Parse` directly
desugarNameDef (P.TypeAnnot name ty) = TypeAnnot name ty

makeCtorFunction :: (String, [TypeExpr]) -> Def
makeCtorFunction (ctor, ts) = Name $ NameDef ctor $ desugarLambda names body
  where
    name i = "__ctorparam" ++ show i
    names = map name [0..(pred $ length ts)]
    body = CtorApp ctor (Tuple $ map Variable names)

desugarTypeDef :: P.TypeDef -> (TypeDef, [Def])
desugarTypeDef (P.Variant xs) = (typeDef, nameDefs)
  where
    f (ctor, ts) = (ctor, Product ts)
    typeDef = Variant $ map f xs
    nameDefs = map makeCtorFunction xs

desugarDef :: P.Def -> [Def] -> [Def]
desugarDef (P.Name body) acc           = (Name $ desugarNameDef body):acc
desugarDef (P.Type name vars body) acc = (Type name vars typeDef):nameDefs ++ acc
  where
    (typeDef, nameDefs) = desugarTypeDef body

desugar :: P.Code -> Code
desugar = foldr desugarDef []
