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

generic_ptr :: Ty.Type
generic_ptr = Ty.ptr Ty.i8

function_type :: Ty.Type
function_type = Ty.FunctionType generic_ptr [generic_ptr, generic_ptr] False

function_ptr :: Ty.Type
function_ptr = Ty.ptr function_type

const_int :: Int -> AST.Operand
const_int i = AST.ConstantOperand $ Const.Int 64 $ toInteger i

gen_expr :: (IR.MonadIRBuilder m, IR.MonadModuleBuilder m) => [AST.Operand] -> Expr -> m AST.Operand
gen_expr _    (Integer i) = return $ const_int i
gen_expr args (Parameter i) = return $ args !! i
gen_expr _    (FunctionRef i) = return $ AST.ConstantOperand $ Const.GlobalReference function_ptr $ AST.mkName $ name_function i
gen_expr args (Call f a) = do
  f' <- gen_expr args f
  a' <- mapM (gen_expr args) a
  IR.call f' $ map (,[]) a'
gen_expr args (Tuple xs) = do
  xs' <- mapM (gen_expr args) xs
  malloc <- IR.extern (AST.mkName "malloc") [Ty.i64] generic_ptr
  m <- IR.call malloc [(const_int len, [])]
  m <- IR.bitcast m $ Ty.ptr generic_ptr
  forM_ (zip [0..] xs') $ \(i, x) -> do
    e <- IR.gep m [const_int i]
    IR.store e 0 x
  m <- IR.bitcast m $ generic_ptr
  return m
  where
    len = length xs

gen_function :: IR.MonadModuleBuilder m => String -> Function -> m AST.Operand
gen_function name (Function expr) =
  IR.function (AST.mkName name) [(generic_ptr, "env"), (generic_ptr, "param")] generic_ptr $ \args -> do
    IR.ret =<< gen_expr args expr

name_function :: Int -> String
name_function i = "__faber_fn_" ++ show i

codegen :: Module -> AST.Module
codegen m = IR.buildModule "faber-output" $ do
  zipWithM_ (gen_function . name_function) [0..] (functions m)
  IR.function "main" [(Ty.i32, "argc"), (Ty.ptr (Ty.ptr Ty.i8), "argv")] Ty.i32 $ \[_, _] -> do
    IR.ret $ AST.ConstantOperand $ Const.Int 32 0

to_llvm :: AST.Module -> IO Text
to_llvm m = LLVM.withContext $ \ctx -> do
  decodeUtf8 <$> LLVM.withModuleFromAST ctx m LLVM.moduleLLVMAssembly
