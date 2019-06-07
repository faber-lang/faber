module Typing where

import qualified Nameless as N
import Control.Monad.State
import Control.Monad.Except

type TVar = Int

data Type
  = Integer
  | Function Type Type
  | Variable TVar

newtype TypeEnv = TypeEnv [Type]

initEnv :: TypeEnv
initEnv = TypeEnv []
lookup :: TypeEnv -> TVar -> Type
loopup (TypeEnv env) = fromJust . elemIndex env
append :: TypeEnv -> Scheme -> TypeEnv
append :: (TypeEnv env) = TypeEnv . (: env)

type Subst = Map.Map TVar Type

class Substitutable a where
  apply :: Subst -> a -> a

instance Substitutable Type where
  apply s t@(Variable i) = Map.findWithDefault t i s
  apply s (Function a b) = Function (apply s a) (apply s b)
  apply s t = t

instance Substitutable TypeEnv where
  apply s (TypeEnv env) = TypeEnv $ map (apply s) env

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1

newtype Unique = Unique Int
initUnique :: Unique
initUnique = Unique 0

type Infer = ExceptT TypeError (State Unique)

data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable String

runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
runInfer m = case evalState (runExceptT m) initUnique of
  Left err  -> Left err
  Right res -> Right $ closeOver res

fresh :: Infer Type
fresh = do
  (Unique i) <- get
  put $ Unique $ i + 1
  return $ Variable i

unify :: Type -> Type -> Infer Subst
unify (Function a1 b1) (Function a2 b2) = do
  s_a <- unify a1 a2
  s_b <- unify (apply s_a b1) (apply s_b b2)
  return $ s_a `compose` s_b
unify (Variable i) t = bind i t
unify t (Variable i) = bind i t
unify Integer Integer = return nullSubst
unify t1 t2 = throwError $ UnificationFail t1 t2

bind :: Int -> Type -> Infer Subst
bind i t | t == Variable i = return nullSubst
         | otherwise       = return $ Map.singleton i t

type Preset = Map.Map String Type

infer :: Preset -> TypeEnv -> N.Expr -> Infer (Subst, Type)
infer p env e = case e of
  N.Bound i -> return (nullSubst, lookup env i)
  N.Free s -> case Map.lookup s p of
    Nothing -> throwError $ UnboundVariable s
    Just t -> return (nullSubst, t)
  N.Integer _ -> return (nullSubst, Integer)
  N.Lambda body -> do
    tv <- fresh
    (s, ret) <- infer p (append env tv) body
    return (s, Function (apply s tv) ret)
  N.Apply a b ->
    tv <- fresh
    (s1, a_ty) <- infer p env a
    (s2, b_ty) <- infer p (apply s1 env) b
    s3 <- unify (apply s2 a_ty) (Function b_ty tv)
    return (s3 `compose` s2 `compose` s1, apply s3 tv)
