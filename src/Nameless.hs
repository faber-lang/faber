module Nameless where

import Control.Monad.Reader
import Data.List            (elemIndex)

import qualified Desugar   as D
import qualified Operators as Op
import           Utils

data Expr
  = Integer Int
  | Lambda Expr
  | Apply Expr Expr
  | ParamBound Int
  | LetBound LetIndex
  | GlobalBound String
  | BinaryOp Op.BinaryOp Expr Expr
  | SingleOp Op.SingleOp Expr
  | Tuple [Expr]
  | LetIn [Expr] Expr
  deriving (Show, Eq)

data DefBody
  = Name Expr
  deriving (Show, Eq)

data Def = Def String DefBody deriving (Show, Eq)

type Code = [Def]

-- types for the conversion
data Binding
  = Global String
  | Param String
  | Let [String]

type Env = [Binding]
initEnv :: Env
initEnv = []
withBinding :: Binding -> Reader Env m -> Reader Env m
withBinding new = local (new:)

data FindState =
  FindState { stLambdaIndex :: Int
            , stLocalIndex  :: Int
            , stLetIndex    :: Int }
initState :: FindState
initState = FindState 0 0 0
withNewLambda :: Finder a -> Finder a
withNewLambda = local update
  where
    -- reset localI in new lambda
    update (FindState lamI _ letI) = FindState (succ lamI) 0 letI
withNewLet :: Finder a -> Finder a
withNewLet = local update
  where
    update (FindState lamI localI letI) = FindState lamI (succ localI) (succ letI)

type Finder = Reader FindState
findInEnv :: Env -> String -> Finder Expr
findInEnv (Param x:xs) s  | x == s    = asks $ ParamBound . stLambdaIndex
                          | otherwise = withNewLambda $ findInEnv xs s
findInEnv (Global x:xs) s | x == s    = return $ GlobalBound s
                          | otherwise = findInEnv xs s
findInEnv (Let bs:xs) s =
  case s `elemIndex` bs of
    Just i -> do
      st <- ask
      return $ LetBound $ LetIndex (stLambdaIndex st) (stLocalIndex st) (stLetIndex st) i
    Nothing -> withNewLet $ findInEnv xs s
findInEnv [] s = error $ "Unbound variable " ++ s

type Nameless = Reader Env
findName :: String -> Nameless Expr
findName s = do
  env <- ask
  return $ runReader (findInEnv env s) initState

namelessExpr :: D.Expr -> Nameless Expr
namelessExpr (D.Apply fn arg) = Apply <$> namelessExpr fn <*> namelessExpr arg
namelessExpr (D.Lambda p body) = Lambda <$> withBinding (Param p) (namelessExpr body)
namelessExpr (D.LetIn defs body) = LetIn <$> defs' <*> withBinding (Let names) (namelessExpr body)
  where
    extractBody (D.Def _ (D.Name b)) = b
    extractName (D.Def name (D.Name _)) = name
    defs' = mapM (namelessExpr . extractBody) defs
    names = map extractName defs
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
