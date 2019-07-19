{-# LANGUAGE TemplateHaskell #-}

module Typing where

import           Control.Lens              hiding (Level)
import           Control.Monad.Except
import           Control.Monad.Extra       (fromMaybeM, maybeM)
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Data.Foldable
import qualified Data.Map                  as Map
import           Data.Maybe                (fromMaybe)
import qualified Data.Set                  as Set
import           Data.Tuple.Extra

import qualified Nameless as N
import qualified Parse    as P
import           Utils

type TVar = Int

data Level
  = Free Int
  | Bound
  deriving (Show, Eq)

data Type
  = Integer
  | Variable TVar Level
  | Apply Type Type
  | Enum String
  | Arrow
  | Tuple [Type]
  deriving (Show, Eq)

functionTy :: Type -> Type -> Type
functionTy a = Apply (Apply Arrow a)

data Scheme = Forall [TVar] Type

data TypeEnv =
  TypeEnv { _params  :: [Type]
          , _locals  :: [[Scheme]]
          , _globals :: Map.Map String Scheme }
makeLenses ''TypeEnv

initEnv :: TypeEnv
initEnv = TypeEnv [] [] Map.empty
lookupParam :: Int -> TypeEnv -> Type
lookupParam i = views params (!! i)
lookupLocal :: Int -> Int -> TypeEnv -> Scheme
lookupLocal i1 i2  = views locals ((!! i2) . (!! i1))
lookupGlobal :: String -> TypeEnv -> Maybe Scheme
lookupGlobal name = views globals (Map.lookup name)
appendParam :: Type -> TypeEnv -> TypeEnv
appendParam t = over params (t:)
appendLocal :: [Scheme] -> TypeEnv -> TypeEnv
appendLocal t = over locals (t:)
appendGlobal :: String -> Scheme -> TypeEnv -> TypeEnv
appendGlobal k v = over globals (Map.insert k v)

type Subst = Map.Map TVar Type

nullSubst :: Subst
nullSubst = Map.empty

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set Int

instance Substitutable Type where
  apply s t@(Variable i _) = Map.findWithDefault t i s
  apply s (Apply a b)      = Apply (apply s a) (apply s b)
  apply s (Tuple xs)       = Tuple $ apply s xs
  apply _ Integer          = Integer
  apply _ Arrow            = Arrow
  apply _ (Enum n)         = Enum n

  ftv (Apply a b)    = ftv a `Set.union` ftv b
  ftv (Variable i _) = Set.singleton i
  ftv (Tuple xs)     = ftv xs
  ftv Integer        = Set.empty
  ftv Arrow          = Set.empty
  ftv (Enum _)       = Set.empty

instance Substitutable Scheme where
  apply s (Forall as t) = Forall as $ apply (foldr Map.delete s as) t
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable a => Substitutable [a] where
  apply = map . apply
  ftv = foldr (Set.union . ftv) Set.empty

instance Substitutable TypeEnv where
  apply s (TypeEnv ps ls gs) = TypeEnv (apply s ps) (apply s ls) (Map.map (apply s) gs)
  ftv (TypeEnv ps ls gs) = ftv ps `Set.union` ftv ls `Set.union` ftv (Map.elems gs)

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1

type EvalEnv = Map.Map String Type
initEvalEnv :: EvalEnv
initEvalEnv = Map.fromList [("Int", Integer)]

type CtorEnv = Map.Map String Scheme
initCtorEnv :: CtorEnv
initCtorEnv = Map.empty

data InferReader =
  InferReader { _typeEnv  :: TypeEnv
              , _letLevel :: Int }
makeLenses ''InferReader

initInferReader :: InferReader
initInferReader = InferReader initEnv 0

data Constraint
  = Unify Type Type
  | UnifyAnnot Type Type

data InferState =
  InferState { _unique  :: Int
             , _ctorEnv :: CtorEnv
             , _evalEnv :: EvalEnv
             , _cstrs   :: [Constraint] }
makeLenses ''InferState

initInferState :: InferState
initInferState = InferState 0 initCtorEnv initEvalEnv []

type Infer = ExceptT TypeError (ReaderT InferReader (State InferState))

data TypeError
  = UnificationFail Type Type
  | RigidUnificationFail Int Type
  | InfiniteType Int Type
  | UnboundVariable String
  | UnboundTypeIdentifier String
  deriving (Show, Eq)

runInfer :: Infer a -> Either TypeError a
runInfer m = case evalState (runReaderT (runExceptT m) initInferReader) initInferState of
  Left err -> Left err
  Right a  -> Right a

fresh :: Level -> Infer Type
fresh level = unique += 1 >> uses unique (flip Variable level)

freshFree :: Infer Type
freshFree = fresh =<< views letLevel Free

freshBound :: Infer Type
freshBound = fresh Bound

findParam :: Int -> Infer Type
findParam i = views typeEnv (lookupParam i)

findLocal :: N.LetIndex -> Infer Scheme
findLocal (N.LetIndex _ local inner) = views typeEnv (lookupLocal local inner)

findGlobal :: String -> Infer Scheme
findGlobal s = fromMaybeM (throwError $ UnboundVariable s) $ lookupGlobal s <$> view typeEnv

withParam :: Type -> Infer a -> Infer a
withParam = locally typeEnv . appendParam

withGlobal :: String -> Scheme -> Infer a -> Infer a
withGlobal name = locally typeEnv . appendGlobal name

withSubst :: Subst -> Infer a -> Infer a
withSubst = locally typeEnv . apply

withLocals :: [Scheme] -> Infer a -> Infer a
withLocals = locally typeEnv . appendLocal

pushLevel :: Infer a -> Infer a
pushLevel = locally letLevel succ

unify :: Type -> Type -> Infer ()
unify t1 t2 = cstrs %= (Unify t1 t2:)

-- annot -> ty -> subst
-- TODO: Refactoring (lots of common code with `unify`)
unifyAnnot :: Type -> Type -> Infer ()
unifyAnnot t1 t2 = cstrs %= (UnifyAnnot t1 t2:)

-- generalization and instantiation
generalizer :: Type -> Infer [Int]
generalizer (Apply a b) = (++) <$> generalizer a <*> generalizer b
generalizer Integer = return []
generalizer Arrow = return []
generalizer (Enum _) = return []
generalizer (Tuple xs) = concat <$> mapM generalizer xs
generalizer v@(Variable i (Free level)) = do
    cLevel <- view letLevel
    if cLevel < level
    then introVar v =<< freshBound
    else return []
  where
    introVar ty v@(Variable i Bound) = unify ty v >> return [i]
generalizer (Variable _ Bound) = return []

generalize :: Type -> Infer Scheme
generalize t = do
  vars <- generalizer t
  return $ Forall vars t

instantiate :: Scheme -> Infer Type
instantiate (Forall xs t) = do
  xs' <- replicateM (length xs) freshFree
  let s = Map.fromList $ zip xs xs'
  return $ apply s t

inferExprs :: [N.Expr] -> Infer [Type]
inferExprs = mapM inferExpr

{-# HLINT ignore inferExpr "Reduce duplication" #-}
inferExpr :: N.Expr -> Infer Type
inferExpr (N.ParamBound i) = findParam i
inferExpr (N.GlobalBound name _) = instantiate =<< findGlobal name
inferExpr (N.LetBound i) = instantiate =<< findLocal i
inferExpr (N.Integer _) = return Integer
inferExpr (N.Lambda body) = do
    tv <- freshFree
    ret <- withParam tv $ inferExpr body
    return $ functionTy tv ret
inferExpr (N.Apply a b) = do
    tv <- freshFree
    a_ty <- inferExpr a
    b_ty <- inferExpr b
    unify a_ty (functionTy b_ty tv)
    return tv
inferExpr (N.LetIn annots defs body) = do
    tys <- mapM mapper annots
    iTys <- withLocals tys $ pushLevel $ inferExprs defs
    schemes <- zipWithM zipper iTys annots
    withLocals schemes $ inferExpr body
  where
    mapper annot = maybeM (Forall [] <$> freshFree) evalScheme $ return annot
    zipper t = maybe (generalize t) (go t)
    go t1 annot = do
      scheme <- evalScheme annot
      let (Forall _ t2) = scheme
      -- we don't need the result of `unifyAnnot`
      -- see the comment below in inferDefs
      unifyAnnot t2 t1 >> return scheme
inferExpr (N.BinaryOp op a b) =
    let op_type = Integer in
    do
      a_ty <- inferExpr a
      b_ty <- inferExpr b
      unify a_ty op_type
      unify b_ty op_type
      return op_type
inferExpr (N.SingleOp op x) =
    let op_type = Integer in
    do
      ty <- inferExpr x
      unify ty op_type
      return op_type
inferExpr (N.Tuple xs) = Tuple <$> inferExprs xs
inferExpr (N.If c t e) = do
  t1 <- inferExpr c
  t2 <- inferExpr t
  t3 <- inferExpr e
  unify t1 Integer   -- TODO: Bool
  unify t2 t3
  return t2
inferExpr (N.NthOf n i e) = do
  ts <- replicateM n freshFree
  t2 <- inferExpr e
  unify (Tuple ts) t2
  return $ ts !! i
inferExpr (N.Error _) = freshFree
inferExpr (N.CtorApp name e) = do
  tv <- freshFree
  t1 <- instantiate =<< uses ctorEnv (Map.! name)
  t2 <- inferExpr e
  unify t1 (functionTy t2 tv)
  return tv
inferExpr (N.DataOf name e) = do
  tv <- freshFree
  t1 <- instantiate =<< uses ctorEnv (Map.! name)
  t2 <- inferExpr e
  unify t1 (functionTy tv t2)
  return tv
inferExpr (N.IsCtor name e) = do
  tv <- freshFree
  t1 <- instantiate =<< uses ctorEnv (Map.! name)
  t2 <- inferExpr e
  unify t1 (functionTy tv t2)
  return Integer  -- TODO: Bool

inferDefs :: Map.Map String Scheme -> [N.NameDef] -> Infer ()
inferDefs sig defs = do
  filledSig <- Map.fromList <$> mapM mapper names
  tys <- foldr (collectNames filledSig) (pushLevel $ inferExprs bodies) names
  zipWithM_ zipper names tys
  where
    extract (N.Name name body) = (name, body)
    (names, bodies) = mapAndUnzip extract defs
    collectNames s name = withGlobal name (s Map.! name)
    mapper name = (,) name <$> fromMaybeM (Forall [] <$> freshFree) (return $ Map.lookup name sig)
    zipper name ty = case Map.lookup name sig of
      -- there is no need to use the resulting subst of `unifyAnnot`
      -- because the resulting type is surely `annot`,
      -- and all we need to do here is to check that `t` is compatible with `annot`
      Just (Forall _ annot) -> void $ unifyAnnot annot ty
      Nothing               -> return ()

inferCode :: N.Code -> Infer ()
inferCode (N.Code sig typeDefs defs) = do
  defineTypes typeDefs
  flip inferDefs defs =<< mapMapM evalScheme sig

defineTypes :: [N.TypeDef] -> Infer ()
defineTypes xs = do
  evalEnv %= flip Map.union names'
  mapM_ defineOne xs
  where
    defineOne :: N.TypeDef -> Infer ()
    defineOne (N.Variant name as ctors) = do
      vars <- replicateM (length as) freshBound
      let et = foldl Apply (Enum name) vars
      let tvs = map extractVar vars
      withNames (Map.fromList $ zip as vars) $ mapM_ (defineCtor tvs et) ctors
    defineCtor :: [TVar] -> Type -> (String, P.TypeExpr) -> Infer ()
    defineCtor vars et (name, ty) = do
      t <- evalType ty
      ctorEnv %= Map.insert name (Forall vars $ functionTy t et)
    extractVar (Variable i _) = i
    extract (N.Variant name _ _) = name
    names = map extract xs
    names' = Map.fromList $ zip names $ map Enum names

-- type evaluator
withNames :: Map.Map String Type -> Infer a -> Infer a
withNames m a = do
  old <- use evalEnv
  evalEnv %= flip Map.union m
  r <- a
  evalEnv .= old
  return r

evalScheme :: P.TypeScheme -> Infer Scheme
evalScheme (P.Forall as x) = do
  vars <- replicateM (length as) freshBound
  Forall (map destruct vars) <$> withNames (Map.fromList $ zip as vars) (evalType x)
  where
    destruct (Variable i _) = i

evalType :: P.TypeExpr -> Infer Type
evalType (P.Ident x) = fromMaybeM (throwError $ UnboundTypeIdentifier x) $ Map.lookup x <$> use evalEnv
evalType (P.Function a b) = functionTy <$> evalType a <*> evalType b
evalType (P.Product xs) = Tuple <$> mapM evalType xs
evalType (P.ApplyTy a b) = Apply <$> evalType a <*> evalType b

-- constraint solver
type Solve = Except TypeError

unifiesMany :: [Type] -> [Type] -> MaybeT Solve Subst
unifiesMany [] [] = return nullSubst
unifiesMany (t1 : ts1) (t2 : ts2) = do
  s1 <- lift $ unifies t1 t2
  s2 <- unifiesMany (apply s1 ts1) (apply s1 ts2)
  return $ s2 `compose` s1
unifiesMany _ _ = mzero

manyOrErr :: TypeError -> [Type] -> [Type] -> Solve Subst
manyOrErr err ts1 ts2 = fromMaybeM (throwError err) $ runMaybeT $ unifiesMany ts1 ts2

unifies :: Type -> Type -> Solve Subst
unifies t1 t2                       | t1 == t2 = return nullSubst
unifies (Variable i _) t            = bind i t
unifies t (Variable i _)            = bind i t
unifies t1@(Apply a1 b1) t2@(Apply a2 b2) = manyOrErr (UnificationFail t1 t2) [a1, b1] [a2, b2]
unifies t1@(Tuple xs) t2@(Tuple ys)       = manyOrErr (UnificationFail t1 t2) xs ys
unifies t1 t2                       = throwError $ UnificationFail t1 t2

bind :: Int -> Type -> Solve Subst
bind i (Variable i' _) | i' == i = return nullSubst
bind i t | occursCheck i t = throwError $ InfiniteType i t
         | otherwise       = return $ Map.singleton i t

occursCheck :: Substitutable a => Int -> a -> Bool
occursCheck i t = i `Set.member` ftv t

typing :: N.Code -> Either TypeError ()
typing = runInfer . inferCode
