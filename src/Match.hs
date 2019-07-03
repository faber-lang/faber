module Match where

import qualified Desugar   as D
import qualified Errors    as Err
import qualified Operators as Op
import           Parse     (Pattern (..), TypeExpr, TypeScheme)

data Expr
  = Integer Int
  | Lambda String Expr
  | CtorApp String Expr
  | Apply Expr Expr
  | Variable String
  | BinaryOp Op.BinaryOp Expr Expr
  | SingleOp Op.SingleOp Expr
  | Tuple [Expr]
  | LetIn [NameDef] Expr
  | If Expr Expr Expr
  | NthOf Int Int Expr
  | Error Err.Error
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

convertPattern :: Int -> Expr -> Expr -> Pattern -> Expr -> Expr
convertPattern d fallback target pat expr = localLet target $ case pat of
  -- `s` must be fresh in rhs of NameDef
  PVar s    -> LetIn [NameDef s target'] expr
  PWildcard -> expr
  PInt i    -> If (BinaryOp Op.Eq target' $ Integer i) expr fallback
  PTuple ps -> foldr (folder $ length ps) expr (zip ps [0..])
  where
    folder len (x, idx) = convertPattern (d+1) fallback (NthOf len idx target') x
    bName = "_match" ++ show d
    localLet a = LetIn [NameDef bName a]
    target' = Variable bName

runConvertPattern :: Expr -> Expr -> Pattern -> Expr -> Expr
runConvertPattern = convertPattern 0

convertExpr :: D.Expr -> Expr
convertExpr (D.Apply fn arg) = Apply (convertExpr fn) (convertExpr arg)
convertExpr (D.Lambda p body) = Lambda p $ convertExpr body
convertExpr (D.CtorApp name e) = CtorApp name $ convertExpr e
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
    matcher t ((p, e):xs) = runConvertPattern (matcher t xs) t p (convertExpr e)
    matcher _ []          = Error Err.MatchFail

convertDef :: D.Def -> Def
convertDef (D.Name (D.NameDef name expr)) = Name (NameDef name $ convertExpr expr)
convertDef (D.Name (D.TypeAnnot name scheme)) = Name (TypeAnnot name scheme)
convertDef (D.Type name vars (D.Variant xs)) = Type name vars $ Variant xs

convertCode :: D.Code -> Code
convertCode = map convertDef

convertMatch :: D.Code -> Code
convertMatch = convertCode
