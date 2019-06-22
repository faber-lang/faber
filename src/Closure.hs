module Closure where

import Control.Monad.State
import Data.List

import qualified Lazy      as L
import qualified Operators as Op

data Expr
  = Integer Int
  | Function Expr
  | GlobalName String
  | Parameter
  | Env
  | Apply Expr Expr
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

data Code =
  Code { definitions :: [Def]
       , entrypoint  :: Expr }

-- holds a set of free variables as a state
type Convert = State [L.Expr]

update :: L.Expr -> Convert Int
update e = do
  fvs <- get
  case elemIndex e fvs of
    Just idx -> return idx
    Nothing -> do
      put $ fvs ++ [e]
      return $ length fvs

-- convert a body of lambda
convertExpr :: L.Expr -> Convert Expr
convertExpr (L.ParamBound 0) = return Parameter
convertExpr (L.ParamBound i) = flip NthOf Env <$> update (L.ParamBound $ i - 1)
convertExpr (L.GlobalBound s) = flip NthOf Env <$> update (L.GlobalBound s)
convertExpr (L.Lambda e) = do
  t <- convertExpr $ L.Tuple fvs
  return $ Tuple [Function body, t]
  where
    (body, fvs) = runState (convertExpr e) []
convertExpr (L.Integer i) = return $ Integer i
convertExpr (L.Apply a b) = Apply <$> convertExpr a <*> convertExpr b
convertExpr (L.BinaryOp op a b) = BinaryOp op <$> convertExpr a <*> convertExpr b
convertExpr (L.SingleOp op x) = SingleOp op <$> convertExpr x
convertExpr (L.Tuple xs) = Tuple <$> mapM convertExpr xs
convertExpr (L.NthOf i x) = NthOf i <$> convertExpr x
convertExpr (L.Ref x) = Ref <$> convertExpr x
convertExpr (L.Assign a b) = Assign <$> convertExpr a <*> convertExpr b
convertExpr (L.Deref x) = Deref <$> convertExpr x
convertExpr (L.If c t e) = If <$> convertExpr c <*> convertExpr t <*> convertExpr e
convertExpr (L.LocalLet a b) = LocalLet <$> convertExpr a <*> convertExpr b
convertExpr L.LetBound = return LetBound

convertTopExpr :: L.Expr -> Expr
convertTopExpr (L.GlobalBound s) = GlobalName s
convertTopExpr e                 = evalState (convertExpr e) []

convertDef :: L.Def -> Def
convertDef (L.Def name (L.Name body)) = Def name $ Name $ convertTopExpr body

convert :: L.Code -> Code
convert (L.Code defs entry)= Code (map convertDef defs) (convertTopExpr entry)
