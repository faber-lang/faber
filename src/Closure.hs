module Closure where

import Control.Monad.State
import qualified Nameless as N
import qualified Operators as Op

-- `Function` constructor directly corresponds to the real function
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

-- holds a list of free variables as a state
type Convert = State [Int]

-- convert a body of lambda
convert' :: N.Expr -> Convert Expr
convert' (N.Bound 0) = return Parameter
convert' (N.Bound i) = do
  modify (idx:)
  return $ NthOf idx Env
  where
    -- index in outer lambda (`modify`...)
    -- and index in environment tuple (`NthOf`...)
    idx = i - 1
convert' (N.Lambda e) = do
  t <- convert' $ N.Tuple $ map N.Bound fvs
  return $ Tuple [Function body, t]
  where
    (body, fvs) = runState (convert' e) []
convert' (N.Integer i) = return $ Integer i
convert' (N.Apply a b) = Apply <$> convert' a <*> convert' b
convert' (N.BinaryOp op a b) = BinaryOp op <$> convert' a <*> convert' b
convert' (N.SingleOp op x) = SingleOp op <$> convert' x
convert' (N.Tuple xs) = Tuple <$> mapM convert' xs

convert :: N.Expr -> Expr
convert e = evalState (convert' e) []
