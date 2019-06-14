module Lazy where

import qualified Nameless  as N
import qualified Operators as Op

data Expr
  = Integer Int
  | Lambda Expr
  | Apply Expr Expr
  | Bound Int
  | BinaryOp Op.BinaryOp Expr Expr
  | SingleOp Op.SingleOp Expr
  | Tuple [Expr]
  | NthOf Int Expr
  | Ref Expr
  | Assign Expr Expr
  | Deref Expr
  | If Expr Expr Expr
  | LocalLet Expr Expr
  | LetBound
  deriving (Show, Eq)

incr_vars :: Int -> N.Expr -> N.Expr
incr_vars n (N.Bound i)   | i >= n    = N.Bound $ i + 1
                          | otherwise = N.Bound i
incr_vars n (N.Integer i)             = N.Integer i
incr_vars n (N.Lambda x)              = N.Lambda $ incr_vars (succ n) x
incr_vars n (N.Apply a b)             = N.Apply (incr_vars n a) (incr_vars n b)
incr_vars n (N.BinaryOp op a b)       = N.BinaryOp op (incr_vars n a) (incr_vars n b)
incr_vars n (N.SingleOp op x)         = N.SingleOp op $ incr_vars n x
incr_vars n (N.Tuple xs)              = N.Tuple $ map (incr_vars n) xs

make_evaled_thunk :: Expr -> Expr
make_evaled_thunk e = Ref $ Tuple [Integer 1, e]

make_thunk :: N.Expr -> Expr
make_thunk e = Ref $ Tuple [Integer 0, code]
  where
    code = Lambda $ NthOf 1 $ Assign (Bound 0) updated
    updated = Tuple [Integer 1, lazy $ incr_vars 0 e]

eval_thunk :: Int -> Expr
eval_thunk i = LocalLet (Deref $ Bound i) $ If cond then_ else_
  where
    cond  = NthOf 0 LetBound
    then_ = NthOf 1 LetBound
    else_ = Apply (NthOf 1 LetBound) (Bound i)

is_value :: N.Expr -> Bool
is_value (N.Integer _)      = True
is_value (N.Tuple _)        = True
is_value (N.Lambda _)       = True
is_value (N.Apply _ _)      = False
is_value (N.Bound _)        = False
is_value (N.BinaryOp _ _ _) = False
is_value (N.SingleOp _ _)   = False

lazy :: N.Expr -> Expr
lazy (N.Apply a (N.Bound i))      = Apply (lazy a) (Bound i)
lazy (N.Apply a b)  | is_value b  = Apply (lazy a) (make_thunk b )
                    | otherwise   = Apply (lazy a) (make_evaled_thunk $ lazy b)
lazy (N.Bound i)                  = eval_thunk i
lazy (N.Integer i)                = Integer i
lazy (N.BinaryOp op a b)          = BinaryOp op (lazy a) (lazy b)
lazy (N.SingleOp op x)            = SingleOp op (lazy x)
lazy (N.Tuple xs)                 = Tuple $ map lazy xs
lazy (N.Lambda body)              = Lambda $ lazy body
