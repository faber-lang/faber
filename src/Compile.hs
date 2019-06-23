module Compile where

import qualified Closure  as Fab
import qualified Codegen  as Fab
import qualified Desugar  as Fab
import qualified Hoist    as Fab
import qualified Lazy     as Fab
import qualified Nameless as Fab
import qualified Parse    as Fab
import qualified Typing   as Fab

import           Data.Either.Extra (mapLeft)
import           Data.Text
import qualified LLVM.AST          as LLVM

data CompileError
  = ParseError Fab.ParseError
  | TypeError Fab.TypeError

compileToModule :: String -> String -> Either CompileError LLVM.Module
compileToModule filename source = do
  ast <- mapLeft ParseError $ Fab.parseCode filename source
  ir  <- return $ conv1 ast
  ()  <- mapLeft TypeError $ Fab.typing ir
  return $ conv2 ir
  where
    conv1 = Fab.nameless . Fab.desugar
    conv2 = Fab.codegen . Fab.hoist . Fab.closure . Fab.lazy
