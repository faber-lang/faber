{-# LANGUAGE TemplateHaskell #-}

module Typing where

import           Control.Lens         hiding (Level)
import           Control.Monad.Except
import           Control.Monad.Extra  (fromMaybeM, maybeM)
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Foldable
import qualified Data.Map             as Map
import           Data.Maybe           (fromMaybe)
import qualified Data.Set             as Set
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

data InferState =
  InferState { _unique  :: Int
             , _ctorEnv :: CtorEnv
             , _evalEnv :: EvalEnv }
makeLenses ''InferState

initInferState :: InferState
initInferState = InferState 0 initCtorEnv initEvalEnv

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

unify :: Type -> Type -> Infer Subst
unify (Variable i _) t = bind i t
unify t (Variable i _) = bind i t
unify Integer Integer = return nullSubst
unify Arrow Arrow     = return nullSubst
unify (Tuple a) (Tuple b) = foldr compose nullSubst <$> zipWithM unify a b
unify (Enum a) (Enum b) | a == b = return nullSubst
unify (Apply a1 b1) (Apply a2 b2) = do
  s_a <- unify a1 a2
  s_b <- unify (apply s_a b1) (apply s_a b2)
  return $ s_a `compose` s_b
unify t1 t2 = throwError $ UnificationFail t1 t2

-- annot -> ty -> subst
-- TODO: Refactoring (lots of common code with `unify`)
unifyAnnot :: Type -> Type -> Infer Subst
unifyAnnot t (Variable i _) = bind i t
unifyAnnot (Variable i _) t = throwError $ RigidUnificationFail i t
unifyAnnot Integer Integer = return nullSubst
unifyAnnot Arrow Arrow = return nullSubst
unifyAnnot (Tuple a) (Tuple b) = foldr compose nullSubst <$> zipWithM unifyAnnot a b
unifyAnnot (Enum a) (Enum b) | a == b = return nullSubst
unifyAnnot (Apply a1 b1) (Apply a2 b2) = do
  s_a <- unifyAnnot a1 a2
  s_b <- unifyAnnot (apply s_a b1) (apply s_a b2)
  return $ s_a `compose` s_b
unifyAnnot t1 t2 = throwError $ UnificationFail t1 t2

bind :: Int -> Type -> Infer Subst
bind i (Variable i' _) | i' == i = return nullSubst
bind i t | occursCheck i t = throwError $ InfiniteType i t
         | otherwise       = return $ Map.singleton i t

occursCheck :: Substitutable a => Int -> a -> Bool
occursCheck i t = i `Set.member` ftv t

-- generalization and instantiation
generalizer :: Type -> Infer Subst
generalizer (Apply a b) = do
  s1 <- generalizer a
  s2 <- generalizer (apply s1 b)
  return $ s1 `compose` s2
generalizer Integer = return nullSubst
generalizer Arrow = return nullSubst
generalizer (Enum _) = return nullSubst
generalizer (Tuple xs) = foldr compose nullSubst <$> mapM generalizer xs
generalizer (Variable i (Free level)) = do
  cLevel <- view letLevel
  if cLevel < level
  then bind i =<< freshBound
  else return nullSubst
generalizer (Variable _ Bound) = return nullSubst

generalize :: Type -> Infer Scheme
generalize t = do
  s <- generalizer t
  return $ Forall (extractAll s) (apply s t)
  where
    extractAll = map extract . Map.elems
    extract (Variable i Bound) = i
    extract _                  = error "unreachable"

instantiate :: Scheme -> Infer Type
instantiate (Forall xs t) = do
  xs' <- replicateM (length xs) freshFree
  let s = Map.fromList $ zip xs xs'
  return $ apply s t

inferExprs :: [N.Expr] -> Infer (Subst, [Type])
inferExprs = foldrM f (nullSubst, [])
  where
    f x (s1, tys) = do
      (s2, t) <- withSubst s1 $ inferExpr x
      return (s1 `compose` s2, t : tys)

inferExpr :: N.Expr -> Infer (Subst, Type)
inferExpr (N.ParamBound i) = (,) nullSubst <$> findParam i
inferExpr (N.GlobalBound name _) = (,) nullSubst <$> (instantiate =<< findGlobal name)
inferExpr (N.LetBound i) = (,) nullSubst <$> (instantiate =<< findLocal i)
inferExpr (N.Integer _) = return (nullSubst, Integer)
inferExpr (N.Lambda body) = do
    tv <- freshFree
    (s, ret) <- withParam tv $ inferExpr body
    return (s, functionTy (apply s tv) ret)
inferExpr (N.Apply a b) = do
    tv <- freshFree
    (s1, a_ty) <- inferExpr a
    (s2, b_ty) <- withSubst s1 $ inferExpr b
    s3 <- unify (apply s2 a_ty) (functionTy b_ty tv)
    return (s3 `compose` s2 `compose` s1, apply s3 tv)
inferExpr (N.LetIn annots defs body) = do
    tys <- mapM mapper annots
    (s1, iTys) <- withLocals tys $ pushLevel $ inferExprs defs
    schemes <- zipWithM zipper iTys annots
    (s2, ty) <-
      withSubst s1 $
        withLocals schemes $
          inferExpr body
    return (s1 `compose` s2, ty)
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
      (s1, a_ty) <- inferExpr a
      (s2, b_ty) <- withSubst s1 $ inferExpr b
      s3 <- unify (apply s2 a_ty) op_type
      s4 <- unify (apply s3 b_ty) op_type
      return (s4 `compose` s3 `compose` s2 `compose` s1, op_type)
inferExpr (N.SingleOp op x) =
    let op_type = Integer in
    do
      (s1, ty) <- inferExpr x
      s2 <- unify (apply s1 ty) op_type
      return (s2 `compose` s1, op_type)
inferExpr (N.Tuple xs) = second Tuple <$> inferExprs xs
inferExpr (N.If c t e) = do
  (s1, t1) <- inferExpr c
  (s2, t2) <- inferExpr t
  (s3, t3) <- inferExpr e
  s4 <- unify t1 Integer   -- TODO: Bool
  s5 <- unify t2 t3
  return (s1 `compose` s2 `compose` s3 `compose` s4 `compose` s5, apply s5 t2)
inferExpr (N.NthOf n i e) = do
  ts <- replicateM n freshFree
  (s1, t2) <- inferExpr e
  s2 <- unify (Tuple ts) t2
  return (s1 `compose` s2, apply s2 $ ts !! i)
inferExpr (N.Error _) = (,) nullSubst <$> freshFree
inferExpr (N.CtorApp name e) = do
  tv <- freshFree
  t1 <- instantiate =<< uses ctorEnv (Map.! name)
  (s1, t2) <- inferExpr e
  s2 <- unify t1 (functionTy t2 tv)
  return (s2 `compose` s1, apply s2 tv)
inferExpr (N.DataOf name e) = do
  tv <- freshFree
  t1 <- instantiate =<< uses ctorEnv (Map.! name)
  (s1, t2) <- inferExpr e
  s2 <- unify t1 (functionTy tv t2)
  return (s1 `compose` s2, apply s2 tv)
inferExpr (N.IsCtor name e) = do
  tv <- freshFree
  t1 <- instantiate =<< uses ctorEnv (Map.! name)
  (s1, t2) <- inferExpr e
  s2 <- unify t1 (functionTy tv t2)
  return (s1 `compose` s2, Integer)  -- TODO: Bool

inferDefs :: Map.Map String Scheme -> [N.NameDef] -> Infer ()
inferDefs sig defs = do
  filledSig <- Map.fromList <$> mapM mapper names
  (s, tys) <- foldr (collectNames filledSig) (pushLevel $ inferExprs bodies) names
  zipWithM_ zipper names (apply s tys)
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

typing :: N.Code -> Either TypeError ()
typing = runInfer . inferCode
