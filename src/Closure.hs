module Closure where

import Control.Exception    (assert)
import Control.Monad.Reader
import Control.Monad.State
import Data.List

import qualified Errors    as Err
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
  | Error Err.Error
  deriving (Show, Eq)

data Def = Name String Expr deriving (Show, Eq)

data Code =
  Code { definitions :: [Def]
       , entrypoint  :: Expr }
  deriving (Show, Eq)

-- holds a set of free variables as a state
type Closure = ReaderT Int (State [F.Expr])

update :: F.Expr -> Closure Int
update e = do
  fvs <- get
  case elemIndex e fvs of
    Just idx -> return idx
    Nothing -> do
      put $ fvs ++ [e]
      return $ length fvs

incrDepth :: Closure a -> Closure a
incrDepth = local succ

-- closure a body of lambda
closureBody :: F.Expr -> Closure Expr
closureBody (F.ParamBound 0) = return Parameter
closureBody (F.ParamBound i) = flip NthOf Env <$> update (F.ParamBound $ i - 1)
closureBody (F.GlobalBound s 0) = return $ GlobalName s
closureBody (F.GlobalBound s i) = flip NthOf Env <$> update (F.GlobalBound s $ i - 1)
closureBody (F.LetBound i) | F.lambdaIndex i == 0 = return $ LetBound $ F.letIndex i
                           | otherwise            = flip NthOf Env <$> (update =<< asks (F.LetBound . decrIndex i))
                           where
                             decrIndex (F.LetIndex lam loc) depth = F.LetIndex (pred lam) (loc - depth)
closureBody (F.Lambda e) = do
  t <- closureBody $ F.Tuple fvs
  return $ Tuple [Function body, t]
  where
    (body, fvs) = runClosureBody e
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
closureBody (F.Error err) = return $ Error err
closureBody (F.LetIn def body) = incrDepth $ LetIn <$> closureBody def <*> closureBody body

runClosureBody :: F.Expr -> (Expr, [F.Expr])
runClosureBody e = runState (runReaderT (closureBody e) 0) []

-- closure a top-level expression
closureExpr :: F.Expr -> Expr
closureExpr e = check $ runClosureBody e
  where
    check (e', fvs) = assert (null fvs) e'

closureDef :: F.Def -> Def
closureDef (F.Name name body) = Name name $ closureExpr body

closure :: F.Code -> Code
closure (F.Code defs entry)= Code (map closureDef defs) (closureExpr entry)
