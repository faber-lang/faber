module Typing where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Foldable
import qualified Data.Map             as Map
import qualified Data.Set             as Set
import           Data.Tuple.Extra

import qualified Nameless as N
import           Utils

type TVar = Int

data Level
  = Free Int
  | Bound
  deriving (Show, Eq)

data Type
  = Integer
  | Function Type Type
  | Variable TVar Level
  | Tuple [Type]
  deriving (Show, Eq)

data Scheme = Forall [TVar] Type

data TypeEnv =
  TypeEnv { params  :: [Type]
          , locals  :: [[Scheme]]
          , globals :: Map.Map String Scheme }

initEnv :: TypeEnv
initEnv = TypeEnv [] [] Map.empty
lookupParam :: TypeEnv -> Int -> Type
lookupParam (TypeEnv env _ _)  = (env !!)
lookupLocal :: TypeEnv -> Int -> Int -> Scheme
lookupLocal (TypeEnv _ env _)  = (!!) . (env !!)
lookupGlobal :: TypeEnv -> String -> Maybe Scheme
lookupGlobal (TypeEnv _ _ env) = flip Map.lookup env
appendParam :: TypeEnv -> Type -> TypeEnv
appendParam (TypeEnv ps ls gs) t = TypeEnv (t:ps) ls gs
appendLocal :: TypeEnv -> [Scheme] -> TypeEnv
appendLocal (TypeEnv ps ls gs) t = TypeEnv ps (t:ls) gs
appendGlobal :: TypeEnv -> String -> Scheme -> TypeEnv
appendGlobal (TypeEnv ps ls gs) k v = TypeEnv ps ls $ Map.insert k v gs

type Subst = Map.Map TVar Type

nullSubst :: Subst
nullSubst = Map.empty

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set Int

instance Substitutable Type where
  apply s t@(Variable i _) = Map.findWithDefault t i s
  apply s (Function a b)   = Function (apply s a) (apply s b)
  apply s (Tuple xs)       = Tuple $ apply s xs
  apply s Integer          = Integer

  ftv (Function a b) = ftv a `Set.union` ftv b
  ftv (Variable i _) = Set.singleton i
  ftv (Tuple xs)     = ftv xs
  ftv Integer        = Set.empty

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

newtype Unique = Unique Int
initUnique :: Unique
initUnique = Unique 0
incrUnique :: Unique -> Unique
incrUnique (Unique i) = Unique $ i + 1

type Infer = ExceptT TypeError (ReaderT (TypeEnv, Int) (State Unique))

data TypeError
  = UnificationFail Type Type
  | InfiniteType Int Type
  | UnboundVariable String
  deriving (Show, Eq)

runInfer :: Infer a -> Either TypeError a
runInfer m = case evalState (runReaderT (runExceptT m) (initEnv, 0)) initUnique of
  Left err -> Left err
  Right a  -> Right a

fresh :: Level -> Infer Type
fresh level = do
  (Unique i) <- get
  modify incrUnique
  return $ Variable i level

freshFree :: Infer Type
freshFree = fresh =<< asks (Free . snd)

freshBound :: Infer Type
freshBound = fresh Bound

findParam :: Int -> Infer Type
findParam i = do
  (env, _) <- ask
  return $ lookupParam env i

findLocal :: LetIndex -> Infer Scheme
findLocal (LetIndex _ _ local inner) = do
  (env, _) <- ask
  return $ lookupLocal env local inner

findGlobal :: String -> Infer Scheme
findGlobal s = do
  (env, _) <- ask
  maybe (throwError $ UnboundVariable s) return $ lookupGlobal env s

withParam :: Type -> Infer a -> Infer a
withParam = local . first . flip appendParam

withGlobal :: String -> Scheme -> Infer a -> Infer a
withGlobal name = local . first . flip3 appendGlobal name

withSubst :: Subst -> Infer a -> Infer a
withSubst = local . first . apply

withLocals :: [Scheme] -> Infer a -> Infer a
withLocals = local . first . flip appendLocal

pushLevel :: Infer a -> Infer a
pushLevel = local $ second succ

unify :: Type -> Type -> Infer Subst
unify (Function a1 b1) (Function a2 b2) = do
  s_a <- unify a1 a2
  s_b <- unify (apply s_a b1) (apply s_a b2)
  return $ s_a `compose` s_b
unify (Variable i _) t = bind i t
unify t (Variable i _) = bind i t
unify Integer Integer = return nullSubst
unify (Tuple a) (Tuple b) = foldr compose nullSubst <$> zipWithM unify a b
unify t1 t2 = throwError $ UnificationFail t1 t2

bind :: Int -> Type -> Infer Subst
bind i (Variable i' _) | i' == i = return nullSubst
bind i t | occursCheck i t = throwError $ InfiniteType i t
         | otherwise       = return $ Map.singleton i t

occursCheck :: Substitutable a => Int -> a -> Bool
occursCheck i t = i `Set.member` ftv t

-- generalization and instantiation
generalizer :: Type -> Infer Subst
generalizer (Function a b) = do
  s1 <- generalizer a
  s2 <- generalizer (apply s1 b)
  return $ s1 `compose` s2
generalizer Integer = return nullSubst
generalizer (Tuple xs) = foldr compose nullSubst <$> mapM generalizer xs
generalizer (Variable i (Free level)) = do
  cLevel <- asks snd
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
inferExpr (N.GlobalBound name) = (,) nullSubst <$> (instantiate =<< findGlobal name)
inferExpr (N.LetBound i) = (,) nullSubst <$> (instantiate =<< findLocal i)
inferExpr (N.Integer _) = return (nullSubst, Integer)
inferExpr (N.Lambda body) = do
    tv <- freshFree
    (s, ret) <- withParam tv $ inferExpr body
    return (s, Function (apply s tv) ret)
inferExpr (N.Apply a b) = do
    tv <- freshFree
    (s1, a_ty) <- inferExpr a
    (s2, b_ty) <- withSubst s1 $ inferExpr b
    s3 <- unify (apply s2 a_ty) (Function b_ty tv)
    return (s3 `compose` s2 `compose` s1, apply s3 tv)
inferExpr (N.LetIn defs body) = do
    tvs <- replicateM (length defs) freshFree
    (s1, tys) <- withLocals (map (Forall []) tvs) $ pushLevel $ inferExprs defs
    s2 <- foldr compose nullSubst <$> zipWithM unify tvs (map (apply s1) tys)
    schemes <- mapM generalize tys
    (s3, ty) <-
      withSubst s2 $
        withLocals schemes $
          inferExpr body
    return (s1 `compose` s2 `compose` s3, ty)
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

inferDefs :: N.Code -> Infer ()
inferDefs (N.Name (N.NameDef name body):xs) = do
  (s, t) <- pushLevel $ inferExpr body
  a <- generalize t
  withGlobal name a $ withSubst s $ inferDefs xs
inferDefs [] = return ()

typing :: N.Code -> Either TypeError ()
typing = runInfer . inferDefs
