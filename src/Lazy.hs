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

incrVars :: Int -> N.Expr -> N.Expr
incrVars n (N.Bound i)   | i >= n    = N.Bound $ i + 1
                         | otherwise = N.Bound i
incrVars n (N.Integer i)             = N.Integer i
incrVars n (N.Lambda x)              = N.Lambda $ incrVars (succ n) x
incrVars n (N.Apply a b)             = N.Apply (incrVars n a) (incrVars n b)
incrVars n (N.BinaryOp op a b)       = N.BinaryOp op (incrVars n a) (incrVars n b)
incrVars n (N.SingleOp op x)         = N.SingleOp op $ incrVars n x
incrVars n (N.Tuple xs)              = N.Tuple $ map (incrVars n) xs

makeEvaledThunk :: Expr -> Expr
makeEvaledThunk e = Ref $ Tuple [Integer 1, e]

makeThunk :: N.Expr -> Expr
makeThunk e = Ref $ Tuple [Integer 0, code]
  where
    code = Lambda $ NthOf 1 $ Assign (Bound 0) updated
    updated = Tuple [Integer 1, lazy $ incrVars 0 e]

evalThunk :: Int -> Expr
evalThunk i = LocalLet (Deref $ Bound i) $ If cond then_ else_
  where
    cond  = NthOf 0 LetBound
    then_ = NthOf 1 LetBound
    else_ = Apply (NthOf 1 LetBound) (Bound i)

isValue :: N.Expr -> Bool
isValue (N.Integer _)      = True
isValue (N.Tuple _)        = True
isValue (N.Lambda _)       = True
isValue (N.Apply _ _)      = False
isValue (N.Bound _)        = False
isValue (N.BinaryOp _ _ _) = False
isValue (N.SingleOp _ _)   = False

lazy :: N.Expr -> Expr
lazy (N.Apply a (N.Bound i))      = Apply (lazy a) (Bound i)
lazy (N.Apply a b)  | isValue b   = Apply (lazy a) (makeEvaledThunk $ lazy b)
                    | otherwise   = Apply (lazy a) (makeThunk b)
lazy (N.Bound i)                  = evalThunk i
lazy (N.Integer i)                = Integer i
lazy (N.BinaryOp op a b)          = BinaryOp op (lazy a) (lazy b)
lazy (N.SingleOp op x)            = SingleOp op (lazy x)
lazy (N.Tuple xs)                 = Tuple $ map lazy xs
lazy (N.Lambda body)              = Lambda $ lazy body
