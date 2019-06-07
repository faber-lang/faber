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

apply :: Subst -> TypeEnv -> TypeEnv
compose :: Subst -> Subst -> Subst

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
