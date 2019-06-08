{-# LANGUAGE OverloadedStrings #-}

module Codegen where

import Control.Monad

import Data.Text
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
generic_ptr = Ty.ptr $ Ty.ptr Ty.i8

gen_expr :: IR.MonadIRBuilder m => AST.Operand -> AST.Operand -> Expr -> m AST.Operand
gen_expr env param (Integer i) = return $ AST.ConstantOperand $ Const.Int 64 $ toInteger i
-- gen_expr env param (FunctionRef i) = return $ 

gen_function :: IR.MonadModuleBuilder m => String -> Function -> m AST.Operand
gen_function name (Function expr) =
  IR.function (AST.mkName name) [(generic_ptr, "env"), (generic_ptr, "param")] generic_ptr $ \[env, param] -> do
    IR.ret =<< gen_expr env param expr

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
