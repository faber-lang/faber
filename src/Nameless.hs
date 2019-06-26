module Nameless where

import           Control.Monad.Extra  (mapMaybeM)
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Foldable        (foldrM)
import           Data.List            (elemIndex)
import qualified Data.Map             as Map

import qualified Desugar   as D
import qualified Operators as Op
import           Parse     (TypeScheme)
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
  | LetIn [Maybe TypeScheme] [Expr] Expr
  | If Expr Expr Expr
  deriving (Show, Eq)

data Def = Name String Expr deriving (Show, Eq)

data Code = Code (Map.Map String TypeScheme) [Def] deriving (Show, Eq)

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

type TypeSig = Map.Map String TypeScheme
type Destructed = ([String], [D.Expr])
destructDefs :: [D.NameDef] -> (Destructed, TypeSig)
destructDefs defs = runState (foldrM f ([], []) defs) Map.empty
  where
    f :: D.NameDef -> Destructed -> State TypeSig Destructed
    f (D.NameDef name body) (names, bodies)     = return (name : names, body : bodies)
    f (D.TypeAnnot name scheme) acc = modify (Map.insert name scheme) >> return acc

namelessExpr :: D.Expr -> Nameless Expr
namelessExpr (D.Apply fn arg) = Apply <$> namelessExpr fn <*> namelessExpr arg
namelessExpr (D.Lambda p body) = Lambda <$> withBinding (Param p) (namelessExpr body)
namelessExpr (D.LetIn defs body) = withBinding (Let names) $ LetIn schemes <$> bodies' <*> namelessExpr body
  where
    ((names, bodies), sig) = destructDefs defs
    bodies' = mapM namelessExpr bodies
    schemes = map (`Map.lookup` sig) names
namelessExpr (D.Variable name) = findName name
namelessExpr (D.Integer i) = return $ Integer i
namelessExpr (D.BinaryOp op a b) = BinaryOp op <$> namelessExpr a <*> namelessExpr b
namelessExpr (D.SingleOp op x) = SingleOp op <$> namelessExpr x
namelessExpr (D.Tuple xs) = Tuple <$> mapM namelessExpr xs
namelessExpr (D.If c t e) = If <$> namelessExpr c <*> namelessExpr t <*> namelessExpr e

namelessNameDef :: D.Def -> Nameless (Maybe Def)
namelessNameDef (D.Name (D.NameDef name expr)) = Just . Name name <$> namelessExpr expr
namelessNameDef _ = return Nothing

namelessDefs :: [D.Def] -> Nameless Code
namelessDefs defs = Code annots <$> foldr collectNames body defs
  where
    body = mapMaybeM namelessNameDef defs
    annots = foldr collectAnnots Map.empty defs
    collectNames (D.Name (D.NameDef name _)) acc = withBinding (Global name) acc
    collectNames (D.Name (D.TypeAnnot _ _)) acc  = acc
    collectAnnots (D.Name (D.NameDef _ _))           = id
    collectAnnots (D.Name (D.TypeAnnot name scheme)) = Map.insert name scheme

namelessCode :: D.Code -> Nameless Code
namelessCode = namelessDefs

nameless :: D.Code -> Code
nameless c = runReader (namelessCode c) initEnv
