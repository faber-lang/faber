module Flatten where

import qualified Operators as Op

data LetIndex =
  LetIndex { lambdaIndex :: Int
           , localIndex  :: Int }
  deriving (Show, Eq)

data Expr
  = Integer Int
  | Lambda Expr
  | Apply Expr Expr
  | ParamBound Int
  | LetBound LetIndex
  | GlobalBound String
  | BinaryOp Op.BinaryOp Expr Expr
  | SingleOp Op.SingleOp Expr
  | Tuple [Expr]
  | NthOf Int Expr
  | Ref (Maybe Expr)
  | Assign Expr Expr
  | Deref Expr
  | If Expr Expr Expr
  | LocalLet Expr Expr
  | LocalBound
  | LetIn Expr Expr
  deriving (Show, Eq)

data Def = Name String Expr deriving (Show, Eq)

data Code =
  Code { definitions :: [Def]
       , entrypoint  :: Expr }
  deriving (Show, Eq)
