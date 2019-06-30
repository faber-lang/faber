module Nameless where

import           Control.Monad.Extra  (mapMaybeM)
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Foldable        (foldrM)
import           Data.List            (elemIndex)
import qualified Data.Map             as Map

import qualified Errors    as Err
import qualified Match     as M
import qualified Operators as Op
import           Parse     (TypeScheme)
import           Utils

data LetIndex =
  LetIndex { lambdaIndex :: Int
           , letIndex    :: Int
           , innerIndex  :: Int }
  deriving (Show, Eq)

mapLambdaIndex :: (Int -> Int) -> LetIndex -> LetIndex
mapLambdaIndex f (LetIndex lamI letI innI) = LetIndex (f lamI) letI innI

data Expr
  = Integer Int
  | Lambda Expr
  | Apply Expr Expr
  | ParamBound Int
  | LetBound LetIndex
  | GlobalBound String Int
  | BinaryOp Op.BinaryOp Expr Expr
  | SingleOp Op.SingleOp Expr
  | Tuple [Expr]
  | LetIn [Maybe TypeScheme] [Expr] Expr
  | If Expr Expr Expr
  | NthOf Int Int Expr
  | Error Err.Error
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
            , stLetIndex    :: Int }
initState :: FindState
initState = FindState 0 0
withNewLambda :: Finder a -> Finder a
withNewLambda = local update
  where
    -- reset localI in new lambda
    update (FindState lamI letI) = FindState (succ lamI) letI
withNewLet :: Finder a -> Finder a
withNewLet = local update
  where
    update (FindState lamI letI) = FindState lamI (succ letI)

type Finder = Reader FindState
findInEnv :: Env -> String -> Finder Expr
findInEnv (Param x:xs) s  | x == s    = asks $ ParamBound . stLambdaIndex
                          | otherwise = withNewLambda $ findInEnv xs s
findInEnv (Global x:xs) s | x == s    = asks $ GlobalBound s . stLambdaIndex
                          | otherwise = findInEnv xs s
findInEnv (Let bs:xs) s =
  case s `elemIndex` bs of
    Just i -> do
      st <- ask
      return $ LetBound $ LetIndex (stLambdaIndex st) (stLetIndex st) i
    Nothing -> withNewLet $ findInEnv xs s
findInEnv [] s = error $ "Unbound variable " ++ s

type Nameless = Reader Env
findName :: String -> Nameless Expr
findName s = do
  env <- ask
  return $ runReader (findInEnv env s) initState

type TypeSig = Map.Map String TypeScheme
type Destructed = ([String], [M.Expr])
destructDefs :: [M.NameDef] -> (Destructed, TypeSig)
destructDefs defs = runState (foldrM f ([], []) defs) Map.empty
  where
    f :: M.NameDef -> Destructed -> State TypeSig Destructed
    f (M.NameDef name body) (names, bodies)     = return (name : names, body : bodies)
    f (M.TypeAnnot name scheme) acc = modify (Map.insert name scheme) >> return acc

namelessExpr :: M.Expr -> Nameless Expr
namelessExpr (M.Apply fn arg) = Apply <$> namelessExpr fn <*> namelessExpr arg
namelessExpr (M.Lambda p body) = Lambda <$> withBinding (Param p) (namelessExpr body)
namelessExpr (M.LetIn defs body) = withBinding (Let names) $ LetIn schemes <$> bodies' <*> namelessExpr body
  where
    ((names, bodies), sig) = destructDefs defs
    bodies' = mapM namelessExpr bodies
    schemes = map (`Map.lookup` sig) names
namelessExpr (M.Variable name) = findName name
namelessExpr (M.Integer i) = return $ Integer i
namelessExpr (M.BinaryOp op a b) = BinaryOp op <$> namelessExpr a <*> namelessExpr b
namelessExpr (M.SingleOp op x) = SingleOp op <$> namelessExpr x
namelessExpr (M.Tuple xs) = Tuple <$> mapM namelessExpr xs
namelessExpr (M.If c t e) = If <$> namelessExpr c <*> namelessExpr t <*> namelessExpr e
namelessExpr (M.NthOf n i e) = NthOf n i <$> namelessExpr e
namelessExpr (M.Error err) = return $ Error err

namelessNameDef :: M.Def -> Nameless (Maybe Def)
namelessNameDef (M.Name (M.NameDef name expr)) = Just . Name name <$> namelessExpr expr
namelessNameDef _ = return Nothing

namelessDefs :: [M.Def] -> Nameless Code
namelessDefs defs = Code annots <$> foldr collectNames body defs
  where
    body = mapMaybeM namelessNameDef defs
    annots = foldr collectAnnots Map.empty defs
    collectNames (M.Name (M.NameDef name _)) acc = withBinding (Global name) acc
    collectNames (M.Name (M.TypeAnnot _ _)) acc  = acc
    collectAnnots (M.Name (M.NameDef _ _))           = id
    collectAnnots (M.Name (M.TypeAnnot name scheme)) = Map.insert name scheme

namelessCode :: M.Code -> Nameless Code
namelessCode = namelessDefs

nameless :: M.Code -> Code
nameless c = runReader (namelessCode c) initEnv
