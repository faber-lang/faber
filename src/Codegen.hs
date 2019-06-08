{-# LANGUAGE OverloadedStrings #-}

module Codegen where

import qualified LLVM.AST                   as AST
import qualified LLVM.AST.Constant          as Const
import qualified LLVM.AST.Type              as Ty
import qualified LLVM.IRBuilder.Module      as IR
import qualified LLVM.IRBuilder.Monad       as IR
import qualified LLVM.IRBuilder.Instruction as IR

import Hoist

codegen :: [Function] -> Expr -> AST.Module
codegen funs e = IR.buildModule "faber-output" $ do
  IR.function "main" [(Ty.i32, "argc"), (Ty.ptr (Ty.ptr Ty.i8), "argv")] Ty.i32 $ \[_, _] -> do
    IR.ret $ AST.ConstantOperand $ Const.Int 32 0
