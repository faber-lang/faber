module Nameless where

import Data.List
import Data.Maybe
import qualified Desugar as D
import qualified Operators as Op

data Expr
  = Integer Int
  | Lambda Expr
  | Apply Expr Expr
  | Free String
  | Bound Int
  | BinaryOp Op.BinaryOp Expr Expr
  | SingleOp Op.SingleOp Expr
  | Tuple [Expr]
  deriving (Show)

nameless' :: [String] -> D.Expr -> Expr
nameless' t (D.Apply fn arg) = Apply (nameless' t fn) (nameless' t arg)
nameless' t (D.Lambda p body) = Lambda $ nameless' (p : t) body
nameless' t (D.Variable name) = maybe (Free name) Bound $ elemIndex name t
nameless' _ (D.Integer i) = Integer i
nameless' t (D.BinaryOp op a b) = BinaryOp op (nameless' t a) (nameless' t b)
nameless' t (D.SingleOp op x) = SingleOp op $ nameless' t x
nameless' t (D.Tuple xs) = Tuple $ map (nameless' t) xs

nameless :: D.Expr -> Expr
nameless = nameless' []
