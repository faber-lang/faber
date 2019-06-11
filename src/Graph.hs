module Graph where

-- Graph reduction machine

import qualified Hoist     as H
import qualified Operators as Op

-- `Graph` express how to construct the graph of expression
data Graph
  = Constant Int
  | FunctionRef Int
  | Parameter Int
  | LetBound
  | Node [Graph]
  deriving (Show, Eq)

-- primitive operations (operands are strictly evaluated)
data PrimKind
  = Add
  | Mul
  | Neg
  deriving (Show, Eq)

-- `[Instruction]` express how to manipulate the graph to achieve target behavior
data Instruction
  = Build Graph
  | MoveTo Int
  | Unwind
  | Return
  | Call Int
  | Tuple
  | NthOf Int
  | Primitive PrimKind
  deriving (Show, Eq)

newtype Function = Function [Instruction] deriving (Show, Eq)

data Program =
  Program { functions  :: [Function]
          , entrypoint :: Function }
  deriving (Show, Eq)

buildGraph :: H.Expr -> Graph
buildGraph (H.Integer i)       = Constant i
buildGraph (H.FunctionRef idx) = FunctionRef idx
buildGraph (H.Parameter n)     = Parameter n
buildGraph (H.Call x xs)       = Node $ buildGraph x : map buildGraph xs
buildGraph (H.BinaryOp _ a b)  = Node [buildGraph a, buildGraph b]
buildGraph (H.SingleOp _ x)    = Node [buildGraph x]
buildGraph (H.Tuple xs)        = Node $ map buildGraph xs
buildGraph (H.NthOf _ e)       = Node [buildGraph e]
buildGraph (H.LocalLet a b)    = Node [buildGraph a, buildGraph b]
buildGraph H.LetBound          = LetBound
