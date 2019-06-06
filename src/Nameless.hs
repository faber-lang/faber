module Nameless where

import Data.List
import Data.Maybe
import qualified Desugar as D

data Expr
  = Integer Int
  | Lambda Expr
  | Apply Expr Expr
  | Free String
  | Bound Int
  deriving (Show)

nameless' :: [String] -> D.Expr -> Expr
nameless' t (D.Apply fn arg) = Apply (nameless' t fn) (nameless' t arg)
nameless' t (D.Lambda p body) = Lambda $ nameless' (p : t) body
nameless' t (D.Variable name) = maybe (Free name) Bound $ elemIndex name t
nameless' _ (D.Integer i) = Integer i

nameless :: D.Expr -> Expr
nameless = nameless' []
