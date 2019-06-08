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
  | LocalLet Expr Expr
  | LetBound
  deriving (Show)

data Module =
  Module { functions :: [Function]
         , entrypoint :: Expr }
  deriving (Show)

type Hoist = State [Function]

hoist_fun :: Expr -> Hoist Expr
hoist_fun e = do
  modify (Function e:)
  FunctionRef <$> pred <$> length <$> get

convert_apply :: Expr -> Expr -> Hoist Expr
convert_apply a b = return $ LocalLet a $ Call (NthOf 0 LetBound) [NthOf 1 LetBound, b]

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
hoist e = Module { functions = reverse funs, entrypoint = e' }
  where
    (e', funs) = runState (hoist' e) []
