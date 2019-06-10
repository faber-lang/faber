module Hoist where

import qualified Closure             as C
import           Control.Monad.State
import qualified Operators           as Op

-- `Call` and `Function` directly correspond to the actual call and function
newtype Function = Function Expr deriving (Show, Eq)

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
  deriving (Show, Eq)

data Module =
  Module { functions  :: [Function]
         , entrypoint :: Expr }
  deriving (Show, Eq)

type Hoist = State [Function]

hoistFun :: Expr -> Hoist Expr
hoistFun e = do
  modify (Function e:)
  gets (FunctionRef . pred <$> length)

convertApply :: Expr -> Expr -> Hoist Expr
convertApply a b = return $ LocalLet a $ Call (NthOf 0 LetBound) [NthOf 1 LetBound, b]

hoist' :: C.Expr -> Hoist Expr
-- function hoisting
hoist' (C.Function e)      = hoistFun =<< hoist' e
-- closure calling convention
hoist' (C.Apply a b)       = join $ convertApply <$> hoist' a <*> hoist' b
hoist' C.Parameter         = return $ Parameter 1
hoist' C.Env               = return $ Parameter 0
-- boring conversion
hoist' (C.Integer i)       = return $ Integer i
hoist' (C.BinaryOp op a b) = BinaryOp op <$> hoist' a <*> hoist' b
hoist' (C.SingleOp op x)   = SingleOp op <$> hoist' x
hoist' (C.Tuple xs)        = Tuple <$> mapM hoist' xs
hoist' (C.NthOf i x)       = NthOf i <$> hoist' x

hoist :: C.Expr -> Module
hoist e = Module { functions = reverse funs, entrypoint = e' }
  where
    (e', funs) = runState (hoist' e) []
