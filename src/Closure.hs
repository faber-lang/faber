module Closure where

import           Control.Monad.State
import qualified Data.Set            as Set

import qualified Lazy      as L
import qualified Operators as Op

data Expr
  = Integer Int
  | Function Expr
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
  deriving (Show, Eq)

-- holds a set of free variables as a state
type Convert = State (Set.Set Int)

-- convert a body of lambda
convert' :: L.Expr -> Convert Expr
convert' (L.Bound 0) = return Parameter
convert' (L.Bound i) = do
  modify (Set.insert idx)
  return $ NthOf idx Env
  where
    -- index in outer lambda (`modify`...)
    -- and index in environment tuple (`NthOf`...)
    idx = i - 1
convert' (L.Lambda e) = do
  t <- convert' $ L.Tuple $ map L.Bound $ Set.elems fvs
  return $ Tuple [Function body, t]
  where
    (body, fvs) = runState (convert' e) Set.empty
convert' (L.Integer i) = return $ Integer i
convert' (L.Apply a b) = Apply <$> convert' a <*> convert' b
convert' (L.BinaryOp op a b) = BinaryOp op <$> convert' a <*> convert' b
convert' (L.SingleOp op x) = SingleOp op <$> convert' x
convert' (L.Tuple xs) = Tuple <$> mapM convert' xs
convert' (L.NthOf i x) = NthOf i <$> convert' x
convert' (L.Ref x) = Ref <$> convert' x
convert' (L.Assign a b) = Assign <$> convert' a <*> convert' b
convert' (L.Deref x) = Deref <$> convert' x
convert' (L.If c t e) = If <$> convert' c <*> convert' t <*> convert' e

convert :: L.Expr -> Expr
convert e = evalState (convert' e) Set.empty
