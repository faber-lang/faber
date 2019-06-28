module Closure where

import Control.Monad.State
import Data.List

import qualified Flatten   as F
import qualified Operators as Op
import           Utils

data Expr
  = Integer Int
  | Function Expr
  | GlobalName String
  | Parameter
  | Env
  | LetBound Int
  | Apply Expr Expr
  | BinaryOp Op.BinaryOp Expr Expr
  | SingleOp Op.SingleOp Expr
  | Tuple [Expr]
  | NthOf Int Expr
  | Alloc
  | Ref Expr
  | Assign Expr Expr
  | Seq Expr Expr
  | Deref Expr
  | If Expr Expr Expr
  | LocalLet Expr Expr
  | LocalBound
  | LetIn Expr Expr
  deriving (Show, Eq)

data Def = Name String Expr deriving (Show, Eq)

data Code =
  Code { definitions :: [Def]
       , entrypoint  :: Expr }

-- holds a set of free variables as a state
type Closure = State [F.Expr]

update :: F.Expr -> Closure Int
update e = do
  fvs <- get
  case elemIndex e fvs of
    Just idx -> return idx
    Nothing -> do
      put $ fvs ++ [e]
      return $ length fvs

-- closure a body of lambda
closureBody :: F.Expr -> Closure Expr
closureBody (F.ParamBound 0) = return Parameter
closureBody (F.ParamBound i) = flip NthOf Env <$> update (F.ParamBound $ i - 1)
closureBody (F.GlobalBound s) = flip NthOf Env <$> update (F.GlobalBound s)
closureBody (F.LetBound i) | F.lambdaIndex i == 0 = return $ LetBound $ F.localIndex i
                           | otherwise            = flip NthOf Env <$> update (F.LetBound $ decrLambdaIndex i)
                           where
                             decrLambdaIndex (F.LetIndex lam loc) = F.LetIndex (pred lam) loc
closureBody (F.Lambda e) = do
  t <- closureBody $ F.Tuple fvs
  return $ Tuple [Function body, t]
  where
    (body, fvs) = runState (closureBody e) []
closureBody (F.Integer i) = return $ Integer i
closureBody (F.Apply a b) = Apply <$> closureBody a <*> closureBody b
closureBody (F.BinaryOp op a b) = BinaryOp op <$> closureBody a <*> closureBody b
closureBody (F.SingleOp op x) = SingleOp op <$> closureBody x
closureBody (F.Tuple xs) = Tuple <$> mapM closureBody xs
closureBody (F.NthOf i x) = NthOf i <$> closureBody x
closureBody (F.Ref x) = Ref <$> closureBody x
closureBody (F.Assign a b) = Assign <$> closureBody a <*> closureBody b
closureBody (F.Seq a b) = Seq <$> closureBody a <*> closureBody b
closureBody (F.Deref x) = Deref <$> closureBody x
closureBody (F.If c t e) = If <$> closureBody c <*> closureBody t <*> closureBody e
closureBody (F.LocalLet a b) = LocalLet <$> closureBody a <*> closureBody b
closureBody F.Alloc = return Alloc
closureBody F.LocalBound = return LocalBound
closureBody (F.LetIn def body) = LetIn <$> closureBody def <*> closureBody body

-- closure a top-level expression
closureExpr :: F.Expr -> Expr
closureExpr (F.ParamBound i) = error $ "Invalid occurrence of parameter " ++ show i
closureExpr (F.LetBound i) | F.lambdaIndex i == 0 = LetBound $ F.localIndex i
                           | otherwise            = error $ "Invalid occurrence of variable " ++ show i
closureExpr (F.GlobalBound s) = GlobalName s
closureExpr (F.Lambda e) = Tuple [Function body, t]
  where
    t = closureExpr $ F.Tuple fvs
    (body, fvs) = runState (closureBody e) []
closureExpr (F.Integer i) = Integer i
closureExpr (F.Apply a b) = Apply (closureExpr a) (closureExpr b)
closureExpr (F.BinaryOp op a b) = BinaryOp op (closureExpr a) (closureExpr b)
closureExpr (F.SingleOp op x) = SingleOp op $ closureExpr x
closureExpr (F.Tuple xs) = Tuple $ map closureExpr xs
closureExpr (F.NthOf i x) = NthOf i $ closureExpr x
closureExpr (F.Ref x) = Ref $ closureExpr x
closureExpr (F.Assign a b) = Assign (closureExpr a) (closureExpr b)
closureExpr (F.Seq a b) = Seq (closureExpr a) (closureExpr b)
closureExpr (F.Deref x) = Deref $ closureExpr x
closureExpr (F.If c t e) = If (closureExpr c) (closureExpr t) (closureExpr e)
closureExpr (F.LocalLet a b) = LocalLet (closureExpr a) (closureExpr b)
closureExpr F.LocalBound = LocalBound
closureExpr F.Alloc = Alloc
closureExpr (F.LetIn def body) = LetIn (closureExpr def) (closureExpr body)

closureDef :: F.Def -> Def
closureDef (F.Name name body) = Name name $ closureExpr body

closure :: F.Code -> Code
closure (F.Code defs entry)= Code (map closureDef defs) (closureExpr entry)
