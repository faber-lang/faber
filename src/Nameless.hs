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
  | If Expr Expr Expr
  deriving (Show, Eq)

data NameDef
  = NameDef String Expr
  deriving (Show, Eq)

newtype Def = Name NameDef deriving (Show, Eq)

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

splitNameDefs :: [D.NameDef] -> ([String], [D.Expr])
splitNameDefs defs = (map extractName defs, map extractBody defs)
  where
    extractBody (D.NameDef _ b) = b
    extractName (D.NameDef name _) = name

namelessExpr :: D.Expr -> Nameless Expr
namelessExpr (D.Apply fn arg) = Apply <$> namelessExpr fn <*> namelessExpr arg
namelessExpr (D.Lambda p body) = Lambda <$> withBinding (Param p) (namelessExpr body)
namelessExpr (D.LetIn defs body) = withBinding (Let names) $ LetIn <$> mapM namelessExpr defs' <*> namelessExpr body
  where
    (names, defs') = splitNameDefs defs
namelessExpr (D.Variable name) = findName name
namelessExpr (D.Integer i) = return $ Integer i
namelessExpr (D.BinaryOp op a b) = BinaryOp op <$> namelessExpr a <*> namelessExpr b
namelessExpr (D.SingleOp op x) = SingleOp op <$> namelessExpr x
namelessExpr (D.Tuple xs) = Tuple <$> mapM namelessExpr xs
namelessExpr (D.If c t e) = If <$> namelessExpr c <*> namelessExpr t <*> namelessExpr e

namelessDefs :: [D.Def] -> Nameless [Def]
namelessDefs defs = foldr folder makeDefs names
  where
    extract (D.Name d) = d
    (names, defs') = splitNameDefs $ map extract defs
    namelessDef' name body = Name . NameDef name <$> namelessExpr body
    makeDefs = zipWithM namelessDef' names defs'
    folder x = withBinding (Global x)

namelessCode :: D.Code -> Nameless Code
namelessCode = namelessDefs

nameless :: D.Code -> Code
nameless c = runReader (namelessCode c) initEnv
