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

data DefBody
  = Name Expr

data Def = Def String DefBody

type Code = [Def]

-- types for the conversion
data Binding
  = Global String
  | Param String

type Env = [Binding]
initEnv :: Env
initEnv = []
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

namelessExpr :: D.Expr -> Nameless Expr
namelessExpr (D.Apply fn arg) = Apply <$> namelessExpr fn <*> namelessExpr arg
namelessExpr (D.Lambda p body) = Lambda <$> withBinding (Param p) (namelessExpr body)
namelessExpr (D.Variable name) = findName name
namelessExpr (D.Integer i) = return $ Integer i
namelessExpr (D.BinaryOp op a b) = BinaryOp op <$> namelessExpr a <*> namelessExpr b
namelessExpr (D.SingleOp op x) = SingleOp op <$> namelessExpr x
namelessExpr (D.Tuple xs) = Tuple <$> mapM namelessExpr xs

namelessDefBody :: D.DefBody -> Nameless DefBody
namelessDefBody (D.Name body) = Name <$> namelessExpr body

namelessDefs :: [D.Def] -> Nameless [Def]
namelessDefs (D.Def name body:xs) = do
  def <- Def name <$> namelessDefBody body
  xs' <- withBinding (Global name) (namelessDefs xs)
  return $ def : xs'
namelessDefs [] = return []

namelessCode :: D.Code -> Nameless Code
namelessCode = namelessDefs

nameless :: D.Code -> Code
nameless c = runReader (namelessCode c) initEnv
