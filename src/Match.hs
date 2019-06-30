module Match where

import qualified Desugar   as D
import qualified Errors    as Err
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
  | NthOf Int Int Expr
  | LocalLet Expr Expr
  | LocalBound
  | Error Err.Error
  deriving (Show, Eq)

data NameDef
  = NameDef String Expr
  | TypeAnnot String TypeScheme
  deriving (Show, Eq)

newtype Def = Name NameDef deriving (Show, Eq)

type Code = [Def]

convertPattern :: Expr -> Expr -> Pattern -> Expr -> Expr
convertPattern fallback target pat expr = LocalLet target $ case pat of
  -- make sure that s is free in target'...
  PVar s    -> LetIn [NameDef s target'] expr
  PWildcard -> expr
  PInt i    -> If (BinaryOp Op.Eq target' $ Integer i) expr fallback
  PTuple ps -> foldr (folder $ length ps) expr (zip ps [0..])
  where
    folder len (x, idx) = convertPattern fallback (NthOf len idx target') x
    target' = LocalBound

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
    matcher _ []          = Error Err.MatchFail

convertDef :: D.Def -> Def
convertDef (D.Name (D.NameDef name expr)) = Name (NameDef name $ convertExpr expr)
convertDef (D.Name (D.TypeAnnot name scheme)) = Name (TypeAnnot name scheme)

convertCode :: D.Code -> Code
convertCode = map convertDef

convertMatch :: D.Code -> Code
convertMatch = convertCode
