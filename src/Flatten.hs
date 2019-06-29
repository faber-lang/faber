module Flatten where

import Control.Exception    (assert)
import Control.Monad.Reader

import qualified Lazy      as L
import qualified Nameless  as N
import qualified Operators as Op
import           Utils

data LetIndex =
  LetIndex { lambdaIndex :: Int
           , localIndex  :: Int }
  deriving (Show, Eq)

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
  | NthOf Int Expr
  | Alloc
  | Ref Expr
  | Assign Expr Expr
  | Seq Expr Expr
  | Deref Expr
  | If Expr Expr Expr
  | LocalLet Expr Expr
  | LocalBound
  | LetIn Expr Expr
  deriving (Show, Eq)

data Def = Name String Expr deriving (Show, Eq)

data Code =
  Code { definitions :: [Def]
       , entrypoint  :: Expr }
  deriving (Show, Eq)

data ReplaceState =
  ReplaceState { stLambdaIndex :: Int
               , stLocalIndex  :: Int
               , stDefCount    :: Int }
initState :: ReplaceState
initState = ReplaceState 0 0 0

type Replace = Reader ReplaceState

withNewLambda :: Replace a -> Replace a
withNewLambda = local update
  where
    update (ReplaceState lamI _ _) = ReplaceState (succ lamI) 0 0

withNewLet :: Int -> Replace a -> Replace a
withNewLet n = local update
  where
    update (ReplaceState lamI letI locI) = ReplaceState lamI (succ letI) (locI + n)

replace :: L.Expr -> Replace L.Expr
replace (L.Integer i) = return $ L.Integer i
replace (L.Lambda body) = withNewLambda $ L.Lambda <$> replace body
replace (L.LetIn defs body) = withNewLet (length defs) $ L.LetIn <$> mapM replace defs <*> replace body
replace (L.Apply a b) = L.Apply <$> replace a <*> replace b
replace (L.ParamBound i) = return $ L.ParamBound i
replace (L.GlobalBound name) = return $ L.GlobalBound name
replace (L.BinaryOp op a b) = L.BinaryOp op <$> replace a <*> replace b
replace (L.SingleOp op x) = L.SingleOp op <$> replace x
replace (L.Tuple xs) = L.Tuple <$> mapM replace xs
replace (L.NthOf n x) = L.NthOf n <$> replace x
replace (L.Ref x) = L.Ref <$> replace x
replace (L.Assign a b) = L.Assign <$> replace a <*> replace b
replace (L.Deref x) = L.Deref <$> replace x
replace (L.If c t e) = L.If <$> replace c <*> replace t <*> replace e
replace (L.LocalLet a b) = L.LocalLet <$> replace a <*> replace b
replace L.LocalBound = return L.LocalBound
replace (L.LetBound i) = asks (conv i)
  where
    conv idx@(N.LetIndex lam _ loc inn) (ReplaceState lamI locI defc)
                | lamI == lam && loc == locI = L.Deref $ L.LetBound $ N.LetIndex lamI 0 (inn + defc) 0
                | otherwise = L.LetBound idx

runReplace :: L.Expr -> L.Expr
runReplace x = runReader (replace x) initState

flattenExpr :: L.Expr -> Expr
flattenExpr (L.Integer i) = Integer i
flattenExpr (L.Lambda body) = Lambda $ flattenExpr body
flattenExpr (L.Apply a b) = Apply (flattenExpr a) (flattenExpr b)
flattenExpr (L.ParamBound i) = ParamBound i
flattenExpr (L.LetBound (N.LetIndex lam _ loc inn)) = assert (inn == 0) $ LetBound $ LetIndex lam loc
flattenExpr (L.GlobalBound name) = GlobalBound name
flattenExpr (L.BinaryOp op a b) = BinaryOp op (flattenExpr a) (flattenExpr b)
flattenExpr (L.SingleOp op x) = SingleOp op $ flattenExpr x
flattenExpr (L.Tuple xs) = Tuple $ map flattenExpr xs
flattenExpr (L.NthOf n x) = NthOf n $ flattenExpr x
flattenExpr (L.Ref x) = Ref $ flattenExpr x
flattenExpr (L.Assign a b) = Assign (flattenExpr a) (flattenExpr b)
flattenExpr (L.Deref x) = Deref $ flattenExpr x
flattenExpr (L.If c t e) = If (flattenExpr c) (flattenExpr t) (flattenExpr e)
flattenExpr (L.LocalLet a b) = LocalLet (flattenExpr a) (flattenExpr b)
flattenExpr L.LocalBound = LocalBound
flattenExpr (L.LetIn defs body) = foldrN alloc assignments (length defs)
  where
    body' = flattenExpr $ runReplace body
    assignments = foldr Seq body' $ imap makeAssign defs
    makeAssign n x = Assign (nthAlloc n) (flattenExpr (runReplace x))
    nthAlloc = LetBound . LetIndex 0
    alloc = LetIn Alloc

replaceGlobal :: L.Expr -> L.Expr
replaceGlobal (L.Integer i) = L.Integer i
replaceGlobal (L.Lambda body) = L.Lambda $ replaceGlobal body
replaceGlobal (L.Apply a b) = L.Apply (replaceGlobal a) (replaceGlobal b)
replaceGlobal (L.ParamBound i) = L.ParamBound i
replaceGlobal (L.LetBound i) = L.LetBound i
replaceGlobal (L.GlobalBound name) = L.Deref $ L.GlobalBound name
replaceGlobal (L.BinaryOp op a b) = L.BinaryOp op (replaceGlobal a) (replaceGlobal b)
replaceGlobal (L.SingleOp op x) = L.SingleOp op $ replaceGlobal x
replaceGlobal (L.Tuple xs) = L.Tuple $ map replaceGlobal xs
replaceGlobal (L.NthOf n x) = L.NthOf n $ replaceGlobal x
replaceGlobal (L.Ref x) = L.Ref $ replaceGlobal x
replaceGlobal (L.Assign a b) = L.Assign (replaceGlobal a) (replaceGlobal b)
replaceGlobal (L.Deref x) = L.Deref $ replaceGlobal x
replaceGlobal (L.If c t e) = L.If (replaceGlobal c) (replaceGlobal t) (replaceGlobal e)
replaceGlobal (L.LocalLet a b) = L.LocalLet (replaceGlobal a) (replaceGlobal b)
replaceGlobal L.LocalBound = L.LocalBound
replaceGlobal (L.LetIn defs body) = L.LetIn (map replaceGlobal defs) (replaceGlobal body)

flatten :: L.Code -> Code
flatten (L.Code defs entry) = Code defs' entry'
  where
    fillAlloc (L.Name name _) = Name name Alloc
    folder (L.Name k v) = Seq (Assign (GlobalBound k) (flattenExpr (replaceGlobal v)))
    defs' = map fillAlloc defs
    entry' = foldr folder (flattenExpr (replaceGlobal entry)) defs
