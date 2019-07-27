module Nameless where

import           Control.Monad.Extra  (mapMaybeM)
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Foldable        (foldrM)
import           Data.List            (elemIndex)
import qualified Data.Map             as Map

import qualified Class     as C
import qualified Errors    as Err
import qualified Operators as Op
import           Parse     (TypeExpr)
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
  | CtorApp String Expr
  | Apply Expr Expr
  | ParamBound Int
  | LetBound LetIndex
  | GlobalBound String Int
  | BinaryOp Op.BinaryOp Expr Expr
  | SingleOp Op.SingleOp Expr
  | Tuple [Expr]
  | LetIn [Maybe C.TypeScheme] [Expr] Expr
  | If Expr Expr Expr
  | NthOf Int Int Expr
  | IsCtor String Expr
  | DataOf String Expr
  | Error Err.Error
  deriving (Show, Eq)

data NameDef
  = Name String Expr deriving (Show, Eq)

data TypeDef
  = Variant String [String] [(String, TypeExpr)]
  deriving (Show, Eq)

data Code =
  Code { annotations :: Map.Map String C.TypeScheme
       , typeDefs    :: [TypeDef]
       , nameDefs    :: [NameDef] }
       deriving (Show, Eq)

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

type TypeSig = Map.Map String C.TypeScheme
type Destructed = ([String], [C.Expr])
destructDefs :: [C.NameDef] -> (Destructed, TypeSig)
destructDefs defs = runState (foldrM f ([], []) defs) Map.empty
  where
    f :: C.NameDef -> Destructed -> State TypeSig Destructed
    f (C.NameDef name body) (names, bodies)     = return (name : names, body : bodies)
    f (C.TypeAnnot name scheme) acc = modify (Map.insert name scheme) >> return acc

namelessExpr :: C.Expr -> Nameless Expr
namelessExpr (C.Apply fn arg) = Apply <$> namelessExpr fn <*> namelessExpr arg
namelessExpr (C.Lambda p body) = Lambda <$> withBinding (Param p) (namelessExpr body)
namelessExpr (C.CtorApp name e) = CtorApp name <$> namelessExpr e
namelessExpr (C.LetIn defs body) = withBinding (Let names) $ LetIn schemes <$> bodies' <*> namelessExpr body
  where
    ((names, bodies), sig) = destructDefs defs
    bodies' = mapM namelessExpr bodies
    schemes = map (`Map.lookup` sig) names
namelessExpr (C.Variable name) = findName name
namelessExpr (C.Integer i) = return $ Integer i
namelessExpr (C.BinaryOp op a b) = BinaryOp op <$> namelessExpr a <*> namelessExpr b
namelessExpr (C.SingleOp op x) = SingleOp op <$> namelessExpr x
namelessExpr (C.Tuple xs) = Tuple <$> mapM namelessExpr xs
namelessExpr (C.If c t e) = If <$> namelessExpr c <*> namelessExpr t <*> namelessExpr e
namelessExpr (C.NthOf n i e) = NthOf n i <$> namelessExpr e
namelessExpr (C.IsCtor n e) = IsCtor n <$> namelessExpr e
namelessExpr (C.DataOf n e) = DataOf n <$> namelessExpr e
namelessExpr (C.Error err) = return $ Error err

namelessNameDef :: C.Def -> Nameless (Maybe NameDef)
namelessNameDef (C.Name (C.NameDef name expr)) = Just . Name name <$> namelessExpr expr
namelessNameDef _ = return Nothing

namelessDefs :: [C.Def] -> Nameless Code
namelessDefs defs = Code annots tDefs <$> foldr collectNames body defs
  where
    body = mapMaybeM namelessNameDef defs
    annots = foldr collectAnnots Map.empty defs
    tDefs = foldr collectTypeDefs [] defs
    collectNames (C.Name (C.NameDef name _)) acc = withBinding (Global name) acc
    collectNames _ acc = acc
    collectAnnots (C.Name (C.TypeAnnot name scheme)) = Map.insert name scheme
    collectAnnots _                                  = id
    collectTypeDefs (C.Type name vars (C.Variant xs)) = (Variant name vars xs:)
    collectTypeDefs _ = id

namelessCode :: C.Code -> Nameless Code
namelessCode = namelessDefs

nameless :: C.Code -> Code
nameless c = runReader (namelessCode c) initEnv
