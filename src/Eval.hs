module Eval where

import qualified Parse as P
import qualified Nameless as N
import qualified Desugar as D
import qualified Typing as T
import qualified Closure as C
import qualified Hoist as H
import qualified Codegen as Gen

import Data.Text.IO as TIO (putStrLn)

eval :: P.Expr -> IO ()
eval x = TIO.putStrLn =<< c
  where
    e = N.nameless $ D.desugar x
    t = case T.typing e of
          Right t -> t
          Left err -> error $ show err
    c = Gen.to_llvm $ Gen.codegen $ H.hoist $ C.convert e
