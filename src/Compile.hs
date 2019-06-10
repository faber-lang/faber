module Compile where

import qualified Parse as P
import qualified Nameless as N
import qualified Desugar as D
import qualified Typing as T
import qualified Closure as C
import qualified Hoist as H
import qualified Codegen as Gen

import Data.Text
import Control.Exception

compile :: P.Expr -> IO (T.Type, Text)
compile x = do
  ir <- Gen.to_llvm c
  t' <- evaluate t
  return (t', ir)
  where
    e = N.nameless $ D.desugar x
    t = case T.typing e of
          Right t -> t
          Left err -> error $ show err
    c = Gen.codegen $ H.hoist $ C.convert e
