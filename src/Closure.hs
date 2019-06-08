module Closure where

import Control.Monad.State
import qualified Nameless as N
import qualified Operators as Op

data Expr
  = Integer Int
  | Function Expr
  | Parameter
  | Env
  | Apply Expr Expr
  | Free String
  | BinaryOp Op.BinaryOp Expr Expr
  | SingleOp Op.SingleOp Expr
  | Tuple [Expr]
  | NthOf Int Expr
  deriving (Show)

type Convert = State [Int]

convert' :: N.Expr -> Convert Expr
convert' (N.Bound 0) = return Parameter
convert' (N.Bound i) = do
  modify (newi:)
  return $ NthOf newi Env
  where
    newi = i - 1
convert' (N.Lambda e) = do
  t <- convert' $ N.Tuple $ map N.Bound fvs
  return $ Tuple [f, t]
  where
    (body, fvs) = runState (convert' e) []
    f = Function body
convert' (N.Integer i) = return $ Integer i
convert' (N.Apply a b) = do
  a' <- convert' a
  b' <- convert' b
  return $ Apply a' b'
convert' (N.BinaryOp op a b) = do
  a' <- convert' a
  b' <- convert' b
  return $ BinaryOp op a' b'
convert' (N.SingleOp op x) = do
  x' <- convert' x
  return $ SingleOp op x'
convert' (N.Tuple xs) = fmap Tuple $ mapM convert' xs

convert :: N.Expr -> Expr
convert e = evalState (convert' e) []
