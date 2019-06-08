module Hoist where

import Control.Monad.State
import qualified Closure as C
import qualified Operators as Op

-- `Call` and `Function` directly correspond to the actual call and function
newtype Function = Function Expr deriving (Show)

data Expr
  = Integer Int
  | Parameter Int
  | FunctionRef Int
  | Call Expr [Expr]
  | BinaryOp Op.BinaryOp Expr Expr
  | SingleOp Op.SingleOp Expr
  | Tuple [Expr]
  | NthOf Int Expr
  deriving (Show)

data Module =
  Module { functions :: [Function]
         , entrypoint :: Function }
  deriving (Show)

type Hoist = State [Function]

hoist_fun :: Expr -> Hoist Expr
hoist_fun e = do
  modify (Function e:)
  FunctionRef <$> length <$> get

convert_apply :: Expr -> Expr -> Hoist Expr
convert_apply a b = do
  ref <- hoist_fun $ Call (NthOf 0 $ Parameter 0) [NthOf 1 $ Parameter 0, b]
  return $ Call ref [a]

hoist' :: C.Expr -> Hoist Expr
-- function hoisting
hoist' (C.Function e) = hoist_fun =<< hoist' e
-- closure calling convention
hoist' (C.Apply a b) = convert_apply <$> hoist' a <*> hoist' b >>= id
hoist' C.Parameter = return $ Parameter 1
hoist' C.Env = return $ Parameter 0
-- boring conversion
hoist' (C.Integer i) = return $ Integer i
hoist' (C.BinaryOp op a b) = BinaryOp op <$> hoist' a <*> hoist' b
hoist' (C.SingleOp op x) = SingleOp op <$> hoist' x
hoist' (C.Tuple xs) = Tuple <$> mapM hoist' xs
hoist' (C.NthOf i x) = NthOf i <$> hoist' x

hoist :: C.Expr -> Module
hoist e = Module { functions = reverse funs, entrypoint = Function e' }
  where
    (e', funs) = runState (hoist' e) []
