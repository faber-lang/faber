module Nameless where

import Control.Monad.Reader

import qualified Desugar   as D
import qualified Operators as Op

data Expr
  = Integer Int
  | Lambda Expr
  | Apply Expr Expr
  | ParamBound Int
  | GlobalBound String
  | BinaryOp Op.BinaryOp Expr Expr
  | SingleOp Op.SingleOp Expr
  | Tuple [Expr]
  deriving (Show, Eq)

data Binding
  = Global String
  | Param String

type Env = [Binding]
withBinding :: Binding -> Reader Env m -> Reader Env m
withBinding new = local (new:)

type Finder = Reader Int
findInEnv :: Env -> String -> Finder Expr
findInEnv (Param x:xs) s  | x == s    = asks ParamBound
                          | otherwise = local succ $ findInEnv xs s
findInEnv (Global x:xs) s | x == s    = return $ GlobalBound s
                          | otherwise = findInEnv xs s
findInEnv [] s = error $ "Unbound variable " ++ s

type Nameless = Reader Env
findName :: String -> Nameless Expr
findName s = do
  env <- ask
  return $ runReader (findInEnv env s) 0

nameless' :: D.Expr -> Nameless Expr
nameless' (D.Apply fn arg) = Apply <$> nameless' fn <*> nameless' arg
nameless' (D.Lambda p body) = Lambda <$> withBinding (Param p) (nameless' body)
nameless' (D.Variable name) = findName name
nameless' (D.Integer i) = return $ Integer i
nameless' (D.BinaryOp op a b) = BinaryOp op <$> nameless' a <*> nameless' b
nameless' (D.SingleOp op x) = SingleOp op <$> nameless' x
nameless' (D.Tuple xs) = Tuple <$> mapM nameless' xs

nameless :: D.Expr -> Expr
nameless e = runReader (nameless' e) []
