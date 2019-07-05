module Compile where

import qualified Closure  as Fab
import qualified Codegen  as Fab
import qualified Desugar  as Fab
import qualified Enum     as Fab
import qualified Flatten  as Fab
import qualified Hoist    as Fab
import qualified Lazy     as Fab
import qualified Match    as Fab
import qualified Nameless as Fab
import qualified Parse    as Fab
import qualified Typing   as Fab

import Data.ByteString
import Data.Either.Extra (mapLeft)

import qualified LLVM.AST     as LLVMAST
import qualified LLVM.Context as LLVM
import qualified LLVM.Module  as LLVM

data CompileError
  = ParseError Fab.ParseError
  | TypeError Fab.TypeError
  deriving Show

compileToModule :: String -> String -> Either CompileError LLVMAST.Module
compileToModule filename source = do
  ast <- mapLeft ParseError $ Fab.parseCode filename source
  let ir = conv1 ast
  ()  <- mapLeft TypeError $ Fab.typing ir
  return $ conv2 ir
  where
    conv1 = Fab.nameless . Fab.convertMatch . Fab.desugar
    conv2 = Fab.codegen . Fab.hoist . Fab.closure . Fab.flatten . Fab.lazy . Fab.convertEnum

moduleToLLVMAssembly :: LLVMAST.Module -> IO ByteString
moduleToLLVMAssembly m = LLVM.withContext $ \ctx -> LLVM.withModuleFromAST ctx m LLVM.moduleLLVMAssembly
