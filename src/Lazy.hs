module Lazy where

import qualified Nameless  as N
import qualified Operators as Op

data Expr
  = Integer Int
  | Lambda Expr
  | Apply Expr Expr
  | ParamBound Int
  | GlobalBound String
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

data DefBody
  = Name Expr

data Def = Def String DefBody

type Code = [Def]

incrVars :: Int -> N.Expr -> N.Expr
incrVars n (N.ParamBound i)   | i >= n    = N.ParamBound $ i + 1
                              | otherwise = N.ParamBound i
incrVars _ (N.GlobalBound s)         = N.GlobalBound s
incrVars _ (N.Integer i)             = N.Integer i
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
    code = Lambda $ NthOf 1 $ Assign (ParamBound 0) updated
    updated = Tuple [Integer 1, lazy $ incrVars 0 e]

evalThunk :: Expr -> Expr
evalThunk e = LocalLet (Deref e) $ If cond then_ else_
  where
    cond  = NthOf 0 LetBound
    then_ = NthOf 1 LetBound
    else_ = Apply (NthOf 1 LetBound) e

isValue :: N.Expr -> Bool
isValue N.Integer{}     = True
isValue N.Tuple{}       = True
isValue N.Lambda{}      = True
isValue N.Apply{}       = False
isValue N.ParamBound{}  = False
isValue N.GlobalBound{} = False
isValue N.BinaryOp{}    = False
isValue N.SingleOp{}    = False

lazy :: N.Expr -> Expr
lazy (N.Apply a (N.ParamBound i))  = Apply (lazy a) (ParamBound i)
lazy (N.Apply a (N.GlobalBound s)) = Apply (lazy a) (GlobalBound s)
lazy (N.Apply a b)  | isValue b    = Apply (lazy a) (makeEvaledThunk $ lazy b)
                    | otherwise    = Apply (lazy a) (makeThunk b)
lazy (N.ParamBound i)              = evalThunk (ParamBound i)
lazy (N.GlobalBound s)             = evalThunk (GlobalBound s)
lazy (N.Integer i)                 = Integer i
lazy (N.BinaryOp op a b)           = BinaryOp op (lazy a) (lazy b)
lazy (N.SingleOp op x)             = SingleOp op (lazy x)
lazy (N.Tuple xs)                  = Tuple $ map lazy xs
lazy (N.Lambda body)               = Lambda $ lazy body
