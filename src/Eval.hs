module Eval where

import qualified Parse as P
import qualified Nameless as N
import qualified Desugar as D

eval :: P.Expr -> N.Expr
eval = N.nameless . D.desugar
