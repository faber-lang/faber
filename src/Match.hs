module Match where

import qualified Desugar   as D
import qualified Operators as Op
import           Parse     (Pattern (..), TypeScheme)

data Expr
  = Integer Int
  | Lambda String Expr
  | Apply Expr Expr
  | Variable String
  | BinaryOp Op.BinaryOp Expr Expr
  | SingleOp Op.SingleOp Expr
  | Tuple [Expr]
  | LetIn [NameDef] Expr
  | If Expr Expr Expr
  | NthOf Int Expr
  | MatchFail
  deriving (Show, Eq)

data NameDef
  = NameDef String Expr
  | TypeAnnot String TypeScheme
  deriving (Show, Eq)

newtype Def = Name NameDef deriving (Show, Eq)

type Code = [Def]

convertPattern :: Expr -> Expr -> Pattern -> Expr -> Expr
convertPattern fallback target pat expr = case pat of
  PVar s    -> LetIn [NameDef s target] expr
  PWildcard -> expr
  PInt i    -> If (BinaryOp Op.Eq target $ Integer i) expr fallback
  PTuple ps -> foldr folder expr (zip ps [0..])
  where
    folder (x, idx) = convertPattern fallback (NthOf idx target) x

convertExpr :: D.Expr -> Expr
convertExpr (D.Apply fn arg) = Apply (convertExpr fn) (convertExpr arg)
convertExpr (D.Lambda p body) = Lambda p $ convertExpr body
convertExpr (D.LetIn defs body) = LetIn (map go defs) (convertExpr body)
  where
    go (D.NameDef name expr)     = NameDef name $ convertExpr expr
    go (D.TypeAnnot name scheme) = TypeAnnot name scheme
convertExpr (D.Variable name) = Variable name
convertExpr (D.Integer i) = Integer i
convertExpr (D.BinaryOp op a b) = BinaryOp op (convertExpr a) (convertExpr b)
convertExpr (D.SingleOp op x) = SingleOp op $ convertExpr x
convertExpr (D.Tuple xs) = Tuple $ map convertExpr xs
convertExpr (D.If c t e) = If (convertExpr c) (convertExpr t) (convertExpr e)
convertExpr (D.Match target arms) = matcher (convertExpr target) arms
  where
    matcher t ((p, e):xs) = convertPattern (matcher t xs) t p (convertExpr e)
    matcher _ []          = MatchFail

