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
  | LocalBound
  deriving (Show, Eq)

data DefBody
  = Name Expr

data Def = Def String DefBody

data Code =
  Code { definitions :: [Def]
       , entrypoint  :: Expr }

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
    cond  = NthOf 0 LocalBound
    then_ = NthOf 1 LocalBound
    else_ = Apply (NthOf 1 LocalBound) e

isValue :: N.Expr -> Bool
isValue N.Integer{}     = True
isValue N.Tuple{}       = True
isValue N.Lambda{}      = True
isValue N.Apply{}       = False
isValue N.ParamBound{}  = False
isValue N.GlobalBound{} = False
isValue N.BinaryOp{}    = False
isValue N.SingleOp{}    = False

lazify :: N.Expr -> Expr
lazify (N.ParamBound i)  = ParamBound i
lazify (N.GlobalBound s) = GlobalBound s
lazify x | isValue x     = makeEvaledThunk $ lazyExpr x
         | otherwise     = makeThunk x

lazyExpr :: N.Expr -> Expr
lazyExpr (N.Apply a b)       = Apply (lazyExpr a) (lazify b)
lazyExpr (N.ParamBound i)    = evalThunk (ParamBound i)
lazyExpr (N.GlobalBound s)   = evalThunk (GlobalBound s)
lazyExpr (N.Integer i)       = Integer i
lazyExpr (N.BinaryOp op a b) = BinaryOp op (lazyExpr a) (lazyExpr b)
lazyExpr (N.SingleOp op x)   = SingleOp op (lazyExpr x)
lazyExpr (N.Tuple xs)        = Tuple $ map lazyExpr xs
lazyExpr (N.Lambda body)     = Lambda $ lazyExpr body

lazyDefBody :: N.DefBody -> DefBody
lazyDefBody (N.Name x) = Name $ lazify x

lazyDef :: N.Def -> Def
lazyDef (N.Def name body) = Def name $ lazyDefBody body

lazy :: N.Code -> Code
lazy code = Code defs entry
  where
    defs = map lazyDef code
    entry = evalThunk (GlobalBound "main")
