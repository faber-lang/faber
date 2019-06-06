module Eval where

import qualified Parse as P
import qualified Nameless as N

eval :: P.Expr -> N.Expr
eval x = N.nameless x
