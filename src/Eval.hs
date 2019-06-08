module Eval where

import qualified Parse as P
import qualified Nameless as N
import qualified Desugar as D
import qualified Typing as T
import qualified Closure as C

eval :: P.Expr -> (T.Type, N.Expr, C.Expr)
eval x = (t, e, c)
  where
    e = N.nameless $ D.desugar x
    t = case T.typing e of
          Right t -> t
          Left err -> error $ show err
    c = C.convert e
