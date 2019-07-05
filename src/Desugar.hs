module Desugar where

import Data.Bifunctor (bimap)

import qualified Operators as Op
import           Parse     (TypeExpr (..), TypeScheme (..))
import qualified Parse     as P

data Pattern
  = PVar String
  | PWildcard
  | PInt Int
  | PCtor String Pattern
  | PTuple [Pattern]
  deriving (Show, Eq)

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
  | Match Expr [(Pattern, Expr)]
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

desugarPattern :: P.Pattern -> Pattern
desugarPattern (P.PVar name)     = PVar name
desugarPattern P.PWildcard       = PWildcard
desugarPattern (P.PInt i)        = PInt i
desugarPattern (P.PTuple xs)     = PTuple $ map desugarPattern xs
desugarPattern (P.PCtor name xs) = PCtor name $ desugarPattern $ P.PTuple xs

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
desugarExpr (P.Match t arms)    = Match (desugarExpr t) (map (bimap desugarPattern desugarExpr) arms)

desugarNameDef :: P.NameDef -> NameDef
desugarNameDef (P.NameDef name ps body []) = NameDef name $ desugarLambda ps $ desugarExpr body
desugarNameDef (P.NameDef name ps body defs) = NameDef name $ LetIn (map desugarNameDef defs) $ desugarLambda ps $ desugarExpr body
-- using `TypeScheme` from `Parse` directly
desugarNameDef (P.TypeAnnot name ty) = TypeAnnot name ty

makeCtorFunction :: String -> [String] -> (String, [TypeExpr]) -> [Def]
makeCtorFunction tyname vars (ctor, ts) = [Name typeAnnot, Name nameDef]
  where
    name i = "__ctorparam" ++ show i
    names = map name [0..(pred $ length ts)]
    body = CtorApp ctor (Tuple $ map Variable names)
    nameDef = NameDef ctor $ desugarLambda names body
    ctorType = foldl ApplyTy (Ident tyname) $ map Ident vars
    scheme = Forall vars $ foldr Function ctorType ts
    typeAnnot = TypeAnnot ctor scheme

desugarTypeDef :: String -> [String] -> P.TypeDef -> (TypeDef, [Def])
desugarTypeDef tyname vars (P.Variant xs) = (typeDef, nameDefs)
  where
    f (ctor, ts) = (ctor, Product ts)
    typeDef = Variant $ map f xs
    nameDefs = concatMap (makeCtorFunction tyname vars) xs

desugarDef :: P.Def -> [Def] -> [Def]
desugarDef (P.Name body) acc           = (Name $ desugarNameDef body):acc
desugarDef (P.Type name vars body) acc = Type name vars typeDef:nameDefs ++ acc
  where
    (typeDef, nameDefs) = desugarTypeDef name vars body

desugar :: P.Code -> Code
desugar = foldr desugarDef []
