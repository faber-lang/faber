module Lazy where

import Control.Monad.Reader
import Data.Bool            (bool)
import Data.Tuple.Extra     (first, second)

import qualified Enum      as E
import qualified Errors    as Err
import qualified Nameless  as N
import qualified Operators as Op

import Utils

data Expr
  = Integer Int
  | Lambda Expr
  | Apply Expr Expr
  | ParamBound Int
  | LetBound N.LetIndex
  | GlobalBound String Int
  | BinaryOp Op.BinaryOp Expr Expr
  | SingleOp Op.SingleOp Expr
  | Tuple [Expr]
  | NthOf Int Expr
  | Ref Expr
  | Assign Expr Expr
  | Deref Expr
  | If Expr Expr Expr
  | LocalLet Expr Expr
  | LocalBound
  | LetIn [Expr] Expr
  | Error Err.Error
  deriving (Show, Eq)

data Def = Name String Expr deriving (Show, Eq)

data Code =
  Code { definitions :: [Def]
       , entrypoint  :: Expr }
  deriving (Show, Eq)

type Lift = Reader (Int, Int)

liftVars :: E.Expr -> Lift E.Expr
liftVars b@(E.ParamBound i) = bool b (E.ParamBound $ i + 1) <$> asks shouldLift
  where
    shouldLift (n, _) = i >= n
liftVars b@(E.LetBound i) = bool b (E.LetBound $ N.mapLambdaIndex succ i) <$> asks (shouldLift i)
  where
    shouldLift (N.LetIndex lamI letI _) (n, m) = lamI > n || (lamI == n && letI >= m)
liftVars (E.GlobalBound s i)       = return $ E.GlobalBound s (i + 1)
liftVars (E.Integer i)             = return $ E.Integer i
liftVars (E.Lambda x)              = E.Lambda <$> local (first succ) (liftVars x)
liftVars (E.Apply a b)             = E.Apply <$> liftVars a <*> liftVars b
liftVars (E.BinaryOp op a b)       = E.BinaryOp op <$> liftVars a <*> liftVars b
liftVars (E.SingleOp op x)         = E.SingleOp op <$> liftVars x
liftVars (E.Tuple xs)              = E.Tuple <$> mapM liftVars xs
liftVars (E.LetIn ts defs body)    = local (second succ) $ E.LetIn ts <$> mapM liftVars defs <*> liftVars body
liftVars (E.If c t e)              = E.If <$> liftVars c <*> liftVars t <*> liftVars e
liftVars (E.NthOf n i e)           = E.NthOf n i <$> liftVars e
liftVars (E.Error err)             = return $ E.Error err

makeEvaledThunk :: Expr -> Expr
makeEvaledThunk e = Ref $ Tuple [Integer 1, e]

makeThunk :: E.Expr -> Expr
makeThunk e = Ref $ Tuple [Integer 0, code]
  where
    code = Lambda $ NthOf 1 $ Assign (ParamBound 0) updated
    updated = Tuple [Integer 1, lazyExpr $ runReader (liftVars e) (0, 0)]

evalThunk :: Expr -> Expr
evalThunk e = LocalLet (Deref e) $ If cond then_ else_
  where
    cond  = NthOf 0 LocalBound
    then_ = NthOf 1 LocalBound
    else_ = Apply (NthOf 1 LocalBound) e

isValue :: E.Expr -> Bool
isValue E.Integer{}     = True
isValue E.Tuple{}       = True
isValue E.Lambda{}      = True
isValue E.Apply{}       = False
isValue E.ParamBound{}  = False
isValue E.LetBound{}    = False
isValue E.GlobalBound{} = False
isValue E.BinaryOp{}    = False
isValue E.SingleOp{}    = False
isValue E.LetIn{}       = False
isValue E.NthOf{}       = False
isValue E.If{}          = False
isValue E.Error{}       = False

lazify :: E.Expr -> Expr
lazify (E.ParamBound i)    = ParamBound i
lazify (E.GlobalBound s i) = GlobalBound s i
lazify (E.LetBound i)      = LetBound i
lazify x | isValue x       = makeEvaledThunk $ lazyExpr x
         | otherwise       = makeThunk x

lazyExpr :: E.Expr -> Expr
lazyExpr (E.Apply a b)         = Apply (lazyExpr a) (lazify b)
lazyExpr (E.ParamBound i)      = evalThunk (ParamBound i)
lazyExpr (E.LetBound i)        = evalThunk (LetBound i)
lazyExpr (E.GlobalBound s i)   = evalThunk (GlobalBound s i)
lazyExpr (E.Integer i)         = Integer i
lazyExpr (E.BinaryOp op a b)   = BinaryOp op (lazyExpr a) (lazyExpr b)
lazyExpr (E.SingleOp op x)     = SingleOp op (lazyExpr x)
lazyExpr (E.Tuple xs)          = Tuple $ map lazify xs
lazyExpr (E.Lambda body)       = Lambda $ lazyExpr body
lazyExpr (E.LetIn _ defs body) = LetIn (map lazify defs) $ lazyExpr body
lazyExpr (E.If c t e)          = If (lazyExpr c) (lazyExpr t) (lazyExpr e)
lazyExpr (E.NthOf _ i e)       = evalThunk $ NthOf i $ lazyExpr e
lazyExpr (E.Error err)         = Error err

lazyDef :: E.NameDef -> Def
lazyDef (E.Name name body) = Name name $ lazify body

lazy :: E.Code -> Code
lazy (E.Code code) = Code defs entry
  where
    defs = map lazyDef code
    entry = evalThunk (GlobalBound "main" 0)
