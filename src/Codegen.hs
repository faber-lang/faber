{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TupleSections     #-}

module Codegen where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Reader

import Data.Maybe         (fromJust)
import Data.Text          (Text)
import Data.Text.Encoding (decodeUtf8)

import qualified LLVM.AST                   as AST
import qualified LLVM.AST.Constant          as Const
import qualified LLVM.AST.IntegerPredicate  as P
import qualified LLVM.AST.Type              as Ty
import qualified LLVM.IRBuilder.Instruction as IR
import qualified LLVM.IRBuilder.Module      as IR
import qualified LLVM.IRBuilder.Monad       as IR

import qualified LLVM.Context as LLVM
import qualified LLVM.Module  as LLVM

import Hoist
import Operators as Op

-- TODO: every function takes two parameters, fix it with typing

genericPtr :: Ty.Type
genericPtr = Ty.ptr Ty.i8

functionType :: Ty.Type
functionType = Ty.FunctionType genericPtr [genericPtr, genericPtr] False

constInt :: Int -> AST.Operand
constInt i = AST.ConstantOperand $ Const.Int 64 $ toInteger i

namedFunction :: Ty.Type -> String -> AST.Operand
namedFunction t = AST.ConstantOperand . Const.GlobalReference (Ty.ptr t) . AST.mkName

callMalloc :: IR.MonadIRBuilder m => AST.Operand -> m AST.Operand
callMalloc len = IR.call malloc [(len, [])]
  where
    malloc = namedFunction (Ty.FunctionType genericPtr [Ty.i64] False) "malloc"

callMalloc' :: IR.MonadIRBuilder m => AST.Operand -> m AST.Operand
callMalloc' len = do
  m <- callMalloc len
  IR.bitcast m $ Ty.ptr genericPtr

data Env = Env { bound :: Maybe AST.Operand, args :: [AST.Operand]}
withBound :: AST.Operand -> Env -> Env
withBound newBound oldEnv = oldEnv { bound = Just newBound }
getBound :: MonadReader Env m => m AST.Operand
getBound = fromJust . bound <$> ask

initEnv :: Env
initEnv = Env Nothing []
initArg :: [AST.Operand] -> Env
initArg xs = initEnv { args = xs }

genExpr :: (IR.MonadIRBuilder m, IR.MonadModuleBuilder m, MonadFix m, MonadReader Env m) => Expr -> m AST.Operand
genExpr (Integer i) = IR.inttoptr (constInt i) genericPtr
genExpr (Parameter i) = (!! i) . args <$> ask
genExpr (FunctionRef i) = IR.bitcast (namedFunction functionType $ nameFunction i) genericPtr
genExpr (Call f a) = do
  f' <- genExpr f
  f' <- IR.bitcast f' $ Ty.ptr functionType
  a' <- mapM genExpr a
  IR.call f' $ map (,[]) $ pad a'
  where
    pad xs = take 2 $ xs ++ repeat (AST.ConstantOperand $ Const.Null genericPtr)
genExpr (Tuple xs) = do
  xs' <- mapM genExpr xs
  m <- callMalloc' $ constInt $ length xs * 8
  forM_ (zip [0..] xs') $ \(i, x) -> do
    e <- IR.gep m [constInt i]
    IR.store e 0 x
  IR.bitcast m genericPtr
genExpr (NthOf i e) = do
  e' <- genExpr e
  e' <- IR.bitcast e' $ Ty.ptr genericPtr
  ptr <- IR.gep e' [constInt i]
  IR.load ptr 0
genExpr (LocalLet e x) = do
  e' <- genExpr e
  local (withBound e') $ genExpr x
genExpr LetBound = getBound
genExpr (BinaryOp op l r) = join $ apply_op <$> genExpr l <*> genExpr r
  where
    apply_op a b = do
      a' <- IR.ptrtoint a Ty.i64
      b' <- IR.ptrtoint b Ty.i64
      x <- opr a' b'
      IR.inttoptr x genericPtr
    opr =
      case op of
        Op.Add -> IR.add
        Op.Mul -> IR.mul

genExpr (SingleOp op e) = apply_op =<< genExpr e
  where
    apply_op v = do
      v' <- IR.ptrtoint v Ty.i64
      x <- opr v'
      IR.inttoptr x genericPtr
    opr =
      case op of
        Op.Negative -> IR.sub $ constInt 0
        Op.Positive -> return

genExpr (Ref e) = do
  e' <- genExpr e
  m <- callMalloc' $ constInt 8
  IR.store m 0 e'
  IR.bitcast m genericPtr

genExpr (Assign l r) = do
  l' <- genExpr l
  r' <- genExpr r
  dest <- IR.bitcast l' $ Ty.ptr genericPtr
  IR.store dest 0 r'
  return r'

genExpr (Deref e) = do
  e' <- genExpr e
  ptr <- IR.bitcast e' $ Ty.ptr genericPtr
  IR.load ptr 0

genExpr (If c t e) = mdo
  c' <- flip IR.ptrtoint Ty.i64 =<< genExpr c
  cond <- IR.icmp P.EQ c' $ constInt 0
  IR.condBr cond ifElse ifThen
  ifThen <- IR.block
  t' <- genExpr t
  IR.br ifExit
  ifElse <- IR.block
  e' <- genExpr e
  IR.br ifExit
  ifExit <- IR.block
  IR.phi [(t', ifThen), (e', ifElse)]

genFunction :: (IR.MonadModuleBuilder m, MonadFix m) => String -> Function -> m AST.Operand
genFunction name (Function n expr) =
  IR.function (AST.mkName name) params genericPtr $ \args ->
    IR.ret =<< runReaderT (genExpr expr) (initArg args)
  where
    params = replicate n (genericPtr, IR.NoParameterName)

nameFunction :: Int -> String
nameFunction i = "__faber_fn_" ++ show i

codegen :: Module -> AST.Module
codegen m = IR.buildModule "faber-output" $ do
  _ <- IR.extern "malloc" [Ty.i64] genericPtr
  zipWithM_ (genFunction . nameFunction) [0..] (functions m)
  IR.function "main" [(Ty.i32, "argc"), (Ty.ptr (Ty.ptr Ty.i8), "argv")] Ty.i32 $ \[_, _] -> do
    ret <- runReaderT (genExpr $ entrypoint m) initEnv
    int <- IR.ptrtoint ret Ty.i64
    IR.ret =<< IR.trunc int Ty.i32

toLLVM :: AST.Module -> IO Text
toLLVM m = LLVM.withContext $ \ctx ->
  decodeUtf8 <$> LLVM.withModuleFromAST ctx m LLVM.moduleLLVMAssembly
