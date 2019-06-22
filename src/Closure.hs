module Closure where

import Control.Monad.State
import Data.List

import qualified Lazy      as L
import qualified Operators as Op
import           Utils

data Expr
  = Integer Int
  | Function Expr
  | GlobalName String
  | Parameter
  | Env
  | LetBound LetIndex
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
  | LocalBound
  | LetIn [Expr] Expr
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
convertBody :: L.Expr -> Convert Expr
convertBody (L.ParamBound 0) = return Parameter
convertBody (L.ParamBound i) = flip NthOf Env <$> update (L.ParamBound $ i - 1)
convertBody (L.GlobalBound s) = flip NthOf Env <$> update (L.GlobalBound s)
convertBody (L.LetBound i) | lambdaIndex i == 0 = return $ LetBound i
                           | otherwise          = flip NthOf Env <$> update (L.LetBound $ mapLambdaIndex pred i)
convertBody (L.Lambda e) = do
  t <- convertBody $ L.Tuple fvs
  return $ Tuple [Function body, t]
  where
    (body, fvs) = runState (convertBody e) []
convertBody (L.Integer i) = return $ Integer i
convertBody (L.Apply a b) = Apply <$> convertBody a <*> convertBody b
convertBody (L.BinaryOp op a b) = BinaryOp op <$> convertBody a <*> convertBody b
convertBody (L.SingleOp op x) = SingleOp op <$> convertBody x
convertBody (L.Tuple xs) = Tuple <$> mapM convertBody xs
convertBody (L.NthOf i x) = NthOf i <$> convertBody x
convertBody (L.Ref x) = Ref <$> convertBody x
convertBody (L.Assign a b) = Assign <$> convertBody a <*> convertBody b
convertBody (L.Deref x) = Deref <$> convertBody x
convertBody (L.If c t e) = If <$> convertBody c <*> convertBody t <*> convertBody e
convertBody (L.LocalLet a b) = LocalLet <$> convertBody a <*> convertBody b
convertBody L.LocalBound = return LocalBound
convertBody (L.LetIn defs body) = LetIn <$> mapM convertBody defs <*> convertBody body

-- convert a top-level expression
convertExpr :: L.Expr -> Expr
convertExpr (L.ParamBound i) = error $ "Invalid occurrence of parameter " ++ show i
convertExpr (L.LetBound i) | lambdaIndex i == 0 = LetBound i
                           | otherwise          = error $ "Invalid occurrence of variable " ++ show i
convertExpr (L.GlobalBound s) = GlobalName s
convertExpr (L.Lambda e) = Tuple [Function body, t]
  where
    t = convertExpr $ L.Tuple fvs
    (body, fvs) = runState (convertBody e) []
convertExpr (L.Integer i) = Integer i
convertExpr (L.Apply a b) = Apply (convertExpr a) (convertExpr b)
convertExpr (L.BinaryOp op a b) = BinaryOp op (convertExpr a) (convertExpr b)
convertExpr (L.SingleOp op x) = SingleOp op $ convertExpr x
convertExpr (L.Tuple xs) = Tuple $ map convertExpr xs
convertExpr (L.NthOf i x) = NthOf i $ convertExpr x
convertExpr (L.Ref x) = Ref $ convertExpr x
convertExpr (L.Assign a b) = Assign (convertExpr a) (convertExpr b)
convertExpr (L.Deref x) = Deref $ convertExpr x
convertExpr (L.If c t e) = If (convertExpr c) (convertExpr t) (convertExpr e)
convertExpr (L.LocalLet a b) = LocalLet (convertExpr a) (convertExpr b)
convertExpr L.LocalBound = LocalBound
convertExpr (L.LetIn defs body) = LetIn (map convertExpr defs) (convertExpr body)

convertDef :: L.Def -> Def
convertDef (L.Def name (L.Name body)) = Def name $ Name $ convertExpr body

convert :: L.Code -> Code
convert (L.Code defs entry)= Code (map convertDef defs) (convertExpr entry)
