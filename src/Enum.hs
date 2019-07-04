module Enum where

import           Control.Monad.State
import qualified Data.Map            as Map

import qualified Errors    as Err
import qualified Nameless  as N
import qualified Operators as Op
import           Parse     (TypeExpr, TypeScheme)
import           Utils

data Expr
  = Integer Int
  | Lambda Expr
  | CtorApp String Expr
  | Apply Expr Expr
  | ParamBound Int
  | LetBound N.LetIndex
  | GlobalBound String Int
  | BinaryOp Op.BinaryOp Expr Expr
  | SingleOp Op.SingleOp Expr
  | Tuple [Expr]
  | LetIn [Maybe TypeScheme] [Expr] Expr
  | If Expr Expr Expr
  | NthOf Int Int Expr
  | IsCtor String Expr
  | DataOf String Expr
  | Error Err.Error
  deriving (Show, Eq)

type Convert = State (Map.Map String Int)

convertExpr :: N.Expr -> Convert Expr
convertExpr (N.ParamBound i)        = return $ ParamBound i
convertExpr (N.LetBound i)          = return $ LetBound i
convertExpr (N.GlobalBound s i)     = return $ GlobalBound s i
convertExpr (N.Integer i)           = return $ Integer i
convertExpr (N.Lambda x)            = Lambda <$> convertExpr x
convertExpr (N.Apply a b)           = Apply <$> convertExpr a <*> convertExpr b
convertExpr (N.BinaryOp op a b)     = BinaryOp op <$> convertExpr a <*> convertExpr b
convertExpr (N.SingleOp op x)       = SingleOp op <$> convertExpr x
convertExpr (N.Tuple xs)            = Tuple <$> mapM convertExpr xs
convertExpr (N.LetIn ts defs body)  = LetIn ts <$> mapM convertExpr defs <*> convertExpr body
convertExpr (N.If c t e)            = If <$> convertExpr c <*> convertExpr t <*> convertExpr e
convertExpr (N.NthOf n i e)         = NthOf n i <$> convertExpr e
convertExpr (N.Error err)           = return $ Error err
convertExpr (N.CtorApp name e)      = do
  idx <- gets (Map.! name)
  e' <- convertExpr e
  return $ Tuple [Integer idx, e']
convertExpr (N.IsCtor name e)      = do
  idx <- gets (Map.! name)
  e' <- convertExpr e
  return $ BinaryOp Op.Eq (NthOf 2 0 e') (Integer idx)
convertExpr (N.DataOf _ e)         = NthOf 2 1 <$> convertExpr e
