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
    updated = Tuple [Integer 1, lazyExpr $ incrVars 0 e]

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

lazyExpr :: N.Expr -> Expr
lazyExpr (N.Apply a (N.ParamBound i))  = Apply (lazyExpr a) (ParamBound i)
lazyExpr (N.Apply a (N.GlobalBound s)) = Apply (lazyExpr a) (GlobalBound s)
lazyExpr (N.Apply a b)  | isValue b    = Apply (lazyExpr a) (makeEvaledThunk $ lazyExpr b)
                        | otherwise    = Apply (lazyExpr a) (makeThunk b)
lazyExpr (N.ParamBound i)              = evalThunk (ParamBound i)
lazyExpr (N.GlobalBound s)             = evalThunk (GlobalBound s)
lazyExpr (N.Integer i)                 = Integer i
lazyExpr (N.BinaryOp op a b)           = BinaryOp op (lazyExpr a) (lazyExpr b)
lazyExpr (N.SingleOp op x)             = SingleOp op (lazyExpr x)
lazyExpr (N.Tuple xs)                  = Tuple $ map lazyExpr xs
lazyExpr (N.Lambda body)               = Lambda $ lazyExpr body

lazyDefBody :: N.DefBody -> DefBody
lazyDefBody (N.Name (N.ParamBound i))  = Name (ParamBound i)
lazyDefBody (N.Name (N.GlobalBound i)) = Name (GlobalBound i)
lazyDefBody (N.Name x)  | isValue x    = Name $ makeEvaledThunk $ lazyExpr x
                        | otherwise    = Name $ makeThunk x

lazyDef :: N.Def -> Def
lazyDef (N.Def name body) = Def name $ lazyDefBody body

lazy :: N.Code -> Code
lazy = map lazyDef
