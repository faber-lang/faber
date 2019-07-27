module Class where

-- implment type classes by transformation
-- [Philip and Stephen, 1988]

import qualified Errors    as Err
import qualified Match     as M
import qualified Operators as Op
import           Parse     (TypeExpr)
import           Utils

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
  | IsCtor String Expr
  | DataOf String Expr
  | Error Err.Error
  deriving (Show, Eq)

-- [TypeConstraint] is gone here
data TypeScheme
  = Forall [String] TypeExpr
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

typeclasses :: M.Code -> Code
typeclasses code = error "TBD"
