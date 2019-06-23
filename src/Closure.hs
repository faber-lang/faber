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
type Closure = State [L.Expr]

update :: L.Expr -> Closure Int
update e = do
  fvs <- get
  case elemIndex e fvs of
    Just idx -> return idx
    Nothing -> do
      put $ fvs ++ [e]
      return $ length fvs

-- closure a body of lambda
closureBody :: L.Expr -> Closure Expr
closureBody (L.ParamBound 0) = return Parameter
closureBody (L.ParamBound i) = flip NthOf Env <$> update (L.ParamBound $ i - 1)
closureBody (L.GlobalBound s) = flip NthOf Env <$> update (L.GlobalBound s)
closureBody (L.LetBound i) | lambdaIndex i == 0 = return $ LetBound i
                           | otherwise          = flip NthOf Env <$> update (L.LetBound $ mapLambdaIndex pred i)
closureBody (L.Lambda e) = do
  t <- closureBody $ L.Tuple fvs
  return $ Tuple [Function body, t]
  where
    (body, fvs) = runState (closureBody e) []
closureBody (L.Integer i) = return $ Integer i
closureBody (L.Apply a b) = Apply <$> closureBody a <*> closureBody b
closureBody (L.BinaryOp op a b) = BinaryOp op <$> closureBody a <*> closureBody b
closureBody (L.SingleOp op x) = SingleOp op <$> closureBody x
closureBody (L.Tuple xs) = Tuple <$> mapM closureBody xs
closureBody (L.NthOf i x) = NthOf i <$> closureBody x
closureBody (L.Ref x) = Ref <$> closureBody x
closureBody (L.Assign a b) = Assign <$> closureBody a <*> closureBody b
closureBody (L.Deref x) = Deref <$> closureBody x
closureBody (L.If c t e) = If <$> closureBody c <*> closureBody t <*> closureBody e
closureBody (L.LocalLet a b) = LocalLet <$> closureBody a <*> closureBody b
closureBody L.LocalBound = return LocalBound
closureBody (L.LetIn defs body) = LetIn <$> mapM closureBody defs <*> closureBody body

-- closure a top-level expression
closureExpr :: L.Expr -> Expr
closureExpr (L.ParamBound i) = error $ "Invalid occurrence of parameter " ++ show i
closureExpr (L.LetBound i) | lambdaIndex i == 0 = LetBound i
                           | otherwise          = error $ "Invalid occurrence of variable " ++ show i
closureExpr (L.GlobalBound s) = GlobalName s
closureExpr (L.Lambda e) = Tuple [Function body, t]
  where
    t = closureExpr $ L.Tuple fvs
    (body, fvs) = runState (closureBody e) []
closureExpr (L.Integer i) = Integer i
closureExpr (L.Apply a b) = Apply (closureExpr a) (closureExpr b)
closureExpr (L.BinaryOp op a b) = BinaryOp op (closureExpr a) (closureExpr b)
closureExpr (L.SingleOp op x) = SingleOp op $ closureExpr x
closureExpr (L.Tuple xs) = Tuple $ map closureExpr xs
closureExpr (L.NthOf i x) = NthOf i $ closureExpr x
closureExpr (L.Ref x) = Ref $ closureExpr x
closureExpr (L.Assign a b) = Assign (closureExpr a) (closureExpr b)
closureExpr (L.Deref x) = Deref $ closureExpr x
closureExpr (L.If c t e) = If (closureExpr c) (closureExpr t) (closureExpr e)
closureExpr (L.LocalLet a b) = LocalLet (closureExpr a) (closureExpr b)
closureExpr L.LocalBound = LocalBound
closureExpr (L.LetIn defs body) = LetIn (map closureExpr defs) (closureExpr body)

closureDef :: L.Def -> Def
closureDef (L.Def name (L.Name body)) = Def name $ Name $ closureExpr body

closure :: L.Code -> Code
closure (L.Code defs entry)= Code (map closureDef defs) (closureExpr entry)
