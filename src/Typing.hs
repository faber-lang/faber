module Typing where

import           Control.Arrow
import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Map             as Map
import qualified Data.Set             as Set
import qualified Nameless             as N

type TVar = Int

data Type
  = Integer
  | Function Type Type
  | Variable TVar
  | Tuple [Type]
  deriving (Show, Eq)

data TypeEnv =
  TypeEnv { params = [Type]
          , globals = Map.Map String Type }

initEnv :: TypeEnv
initEnv = TypeEnv [] Map.empty
lookupParam :: TypeEnv -> TVar -> Type
lookupParam (TypeEnv env _)  = (env !!)
lookupGlobal :: TypeEnv -> String -> Type
lookupGlobal name (TypeEnv _ env) = fromJust . Map.lookup name env
appendParam :: TypeEnv -> Type -> TypeEnv
appendParam (TypeEnv ps gs) t = TypeEnv (t:ps) gs
appendGlobal :: TypeEnv -> String -> Type -> TypeEnv
appendGlobal (TypeEnv ps gs) k v = TypeEnv ps $ Map.insert k v gs

type Subst = Map.Map TVar Type

nullSubst :: Subst
nullSubst = Map.empty

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set Int

instance Substitutable Type where
  apply s t@(Variable i) = Map.findWithDefault t i s
  apply s (Function a b) = Function (apply s a) (apply s b)
  apply s (Tuple xs)     = Tuple $ apply s xs
  apply s Integer        = Integer

  ftv (Function a b) = ftv a `Set.union` ftv b
  ftv (Variable i)   = Set.singleton i
  ftv (Tuple xs)     = ftv xs
  ftv Integer        = Set.empty

instance Substitutable a => Substitutable [a] where
  apply = map . apply
  ftv = foldr (Set.union . ftv) Set.empty

instance Substitutable TypeEnv where
  apply s (TypeEnv ps gs) = TypeEnv $ apply s ps $ Map.map (apply s) gs
  ftv (TypeEnv ps gs) = ftv ps `Set.union` ftv $ Map.elems gs

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1

newtype Unique = Unique Int
initUnique :: Unique
initUnique = Unique 0
incrUnique :: Unique -> Unique
incrUnique (Unique i) = Unique $ i + 1

data InferState =
  InferState { unique :: Unique
             , env :: TypeEnv }
initInferState :: InferState
initInferState = InferState initUnique initEnv

type Infer = ExceptT TypeError (State InferState)
updateUnique :: (Unique -> Unique) -> Infer Unique
updateUnique f = do
  old <- get
  modify update
  return old
  where
    update (InferState u e) = InferState (f u) e

data TypeError
  = UnificationFail Type Type
  | InfiniteType Int Type
  | UnboundVariable String
  deriving (Show, Eq)

runInfer :: Infer (Subst, Type) -> Either TypeError Type
runInfer m = case evalState (runExceptT m) initInferState of
  Left err     -> Left err
  Right (_, t) -> Right t

fresh :: Infer Type
fresh = do
  (Unique i) <- updateUnique incrUnique
  return $ Variable i

unify :: Type -> Type -> Infer Subst
unify (Function a1 b1) (Function a2 b2) = do
  s_a <- unify a1 a2
  s_b <- unify (apply s_a b1) (apply s_a b2)
  return $ s_a `compose` s_b
unify (Variable i) t = bind i t
unify t (Variable i) = bind i t
unify Integer Integer = return nullSubst
unify (Tuple a) (Tuple b) = foldr compose nullSubst <$> zipWithM unify a b
unify t1 t2 = throwError $ UnificationFail t1 t2

bind :: Int -> Type -> Infer Subst
bind i t | t == Variable i = return nullSubst
         | occursCheck i t = throwError $ InfiniteType i t
         | otherwise       = return $ Map.singleton i t

occursCheck :: Substitutable a => Int -> a -> Bool
occursCheck i t = i `Set.member` ftv t

inferExpr :: TypeEnv -> N.Expr -> Infer (Subst, Type)
inferExpr env e = case e of
  N.ParamBound i -> return (nullSubst, lookupParam env i)
  N.GlobalBound name -> return (nullSubst, lookupGlobal env name)
  N.Integer _ -> return (nullSubst, Integer)
  N.Lambda body -> do
    tv <- fresh
    (s, ret) <- infer (appendParam tv) body
    return (s, Function (apply s tv) ret)
  N.Apply a b -> do
    tv <- fresh
    (s1, a_ty) <- infer env a
    (s2, b_ty) <- infer (apply s1 env) b
    s3 <- unify (apply s2 a_ty) (Function b_ty tv)
    return (s3 `compose` s2 `compose` s1, apply s3 tv)
  N.BinaryOp op a b ->
    let op_type = Integer in
    do
      (s1, a_ty) <- infer env a
      (s2, b_ty) <- infer (apply s1 env) b
      s3 <- unify (apply s2 a_ty) op_type
      s4 <- unify (apply s3 b_ty) op_type
      return (s4 `compose` s3 `compose` s2 `compose` s1, op_type)
  N.SingleOp op x ->
    let op_type = Integer in
    do
      (s1, ty) <- infer env x
      s2 <- unify (apply s1 ty) op_type
      return (s2 `compose` s1, op_type)
  N.Tuple xs -> (foldr compose nullSubst *** Tuple) <$> mapAndUnzipM (infer env) xs

typing :: N.Expr -> Either TypeError Type
typing = runInfer . infer initEnv
