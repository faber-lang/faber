{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Codegen where

import Control.Monad

import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

import qualified LLVM.AST                   as AST
import qualified LLVM.AST.Constant          as Const
import qualified LLVM.AST.Type              as Ty
import qualified LLVM.IRBuilder.Module      as IR
import qualified LLVM.IRBuilder.Monad       as IR
import qualified LLVM.IRBuilder.Instruction as IR

import qualified LLVM.Module  as LLVM
import qualified LLVM.Context as LLVM

import Hoist
import Operators as Op

-- TODO: every function takes two parameters, fix it with typing

generic_ptr :: Ty.Type
generic_ptr = Ty.ptr Ty.i8

function_type :: Ty.Type
function_type = Ty.FunctionType generic_ptr [generic_ptr, generic_ptr] False

function_ptr :: Ty.Type
function_ptr = Ty.ptr function_type

const_int :: Int -> AST.Operand
const_int i = AST.ConstantOperand $ Const.Int 64 $ toInteger i

named_function :: Ty.Type -> String -> AST.Operand
named_function t = AST.ConstantOperand . Const.GlobalReference (Ty.ptr t) . AST.mkName

call_malloc :: IR.MonadIRBuilder m => AST.Operand -> m AST.Operand
call_malloc len = IR.call malloc [(len, [])]
  where
    malloc = named_function (Ty.FunctionType generic_ptr [Ty.i64] False) "malloc"

gen_expr :: (IR.MonadIRBuilder m, IR.MonadModuleBuilder m) => [AST.Operand] -> Maybe AST.Operand -> Expr -> m AST.Operand
gen_expr _    _  (Integer i) = IR.inttoptr (const_int i) generic_ptr
gen_expr args _ (Parameter i) = return $ args !! i
gen_expr _    _ (FunctionRef i) = IR.bitcast (named_function function_type $ name_function i) generic_ptr
gen_expr args b (Call f a) = do
  f' <- gen_expr args b f
  f' <- IR.bitcast f' function_ptr
  a' <- mapM (gen_expr args b) a
  IR.call f' $ map (,[]) $ pad a'
  where
    pad xs = take 2 $ xs ++ repeat (AST.ConstantOperand $ Const.Null generic_ptr)
gen_expr args b (Tuple xs) = do
  xs' <- mapM (gen_expr args b) xs
  m <- call_malloc $ const_int $ length xs * 8
  m <- IR.bitcast m $ Ty.ptr generic_ptr
  forM_ (zip [0..] xs') $ \(i, x) -> do
    e <- IR.gep m [const_int i]
    IR.store e 0 x
  m <- IR.bitcast m $ generic_ptr
  return m
gen_expr args b (NthOf i e) = do
  e' <- gen_expr args b e
  e' <- IR.bitcast e' $ Ty.ptr generic_ptr
  ptr <- IR.gep e' [const_int i]
  IR.load ptr 0
gen_expr args b (LocalLet e x) = do
  e' <- gen_expr args b e
  gen_expr args (Just e') x
gen_expr args (Just b) LetBound = return b
gen_expr args Nothing LetBound = error "unbound let binding"
gen_expr args b (BinaryOp op l r) = apply_op <$> gen_expr args b l <*> gen_expr args b r >>= id
  where
    apply_op a b = do
      a' <- IR.ptrtoint a Ty.i64
      b' <- IR.ptrtoint b Ty.i64
      x <- opr a' b'
      IR.inttoptr x generic_ptr
    opr =
      case op of
        Op.Add -> IR.add
        Op.Mul -> IR.mul

gen_expr args b (SingleOp op e) = apply_op =<< gen_expr args b e
  where
    apply_op v = do
      v' <- IR.ptrtoint v Ty.i64
      x <- opr v'
      IR.inttoptr x generic_ptr
    opr =
      case op of
        Op.Negative -> IR.sub $ const_int 0
        Op.Positive -> return

gen_function :: IR.MonadModuleBuilder m => String -> Function -> m AST.Operand
gen_function name (Function expr) =
  IR.function (AST.mkName name) [(generic_ptr, IR.NoParameterName), (generic_ptr, IR.NoParameterName)] generic_ptr $ \args -> do
    IR.ret =<< gen_expr args Nothing expr

name_function :: Int -> String
name_function i = "__faber_fn_" ++ show i

codegen :: Module -> AST.Module
codegen m = IR.buildModule "faber-output" $ do
  _ <- IR.extern "malloc" [Ty.i64] generic_ptr
  zipWithM_ (gen_function . name_function) [0..] (functions m)
  IR.function "main" [(Ty.i32, "argc"), (Ty.ptr (Ty.ptr Ty.i8), "argv")] Ty.i32 $ \[_, _] -> do
    ret <- gen_expr [] Nothing (entrypoint m)
    int <- IR.ptrtoint ret Ty.i64
    IR.ret =<< IR.trunc int Ty.i32

to_llvm :: AST.Module -> IO Text
to_llvm m = LLVM.withContext $ \ctx -> do
  decodeUtf8 <$> LLVM.withModuleFromAST ctx m LLVM.moduleLLVMAssembly
