module Nameless where

import Data.List
import Data.Maybe
import qualified Parse as P

data Expr
  = Integer Int
  | Lambda Expr
  | Apply Expr Expr
  | Free String
  | Bound Int
  deriving (Show)

nameless' :: [P.Ident] -> P.Expr -> Expr
nameless' t (P.Apply fn arg) = Apply (nameless' t fn) (nameless' t arg)
nameless' t (P.Lambda p body) = Lambda $ nameless' (p : t) body
nameless' t (P.Variable name) = maybe (Free name) Bound $ elemIndex name t
nameless' _ (P.Integer i) = Integer i

nameless :: P.Expr -> Expr
nameless = nameless' []
