module Graph where

-- Graph reduction machine

import Control.Monad.State
import Control.Arrow

import qualified Hoist     as H
import qualified Operators as Op

-- `Graph` express how to construct the graph of expression
-- `LocalLet` and `LetBound` forms a looped structure
data Graph
  = Constant Int
  | FunctionRef Int
  | Parameter Int
  | LocalLet Graph Graph
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

data BindState
  = Unbound
  | Bound H.Expr
  | Evaluated

type Builder = State (BindState, [Instruction])

putInst :: Instruction -> Builder ()
putInst i = modify $ second (i:)

putBindState :: BindState -> Builder BindState
putBindState s = do
  (old, xs) <- get
  put (s, xs)
  return old

withBoundExpr :: H.Expr -> Builder () -> Builder ()
withBoundExpr e action = do
  old <- putBindState $ Bound e
  action
  _ <- putBindState $ Unbound
  return ()

takeBoundExpr :: Builder ()
takeBoundExpr = do
  e <- get
  modify $ first $ const

-- build a list of instructions to reduce supplied expression
buildInsts :: H.Expr -> Builder ()
buildInsts (H.Call x xs) = do
  -- reduce lhs first
  putInst $ MoveTo 0
  buildInsts x
  putInst Unwind
  -- then, call
  putInst $ Call $ length xs
buildInsts (H.Tuple xs) = putInst Tuple
-- strict operations
buildInsts (H.BinaryOp op a b) = do
  -- both side of operands are strictly evaluated here
  putInst $ MoveTo 0
  buildInsts a
  putInst Unwind
  putInst $ MoveTo 1
  buildInsts b
  putInst Unwind
  putInst $ Primitive kind
  where
    kind = case op of
      Op.Add -> Add
      Op.Mul -> Mul
buildInsts (H.SingleOp op x) = do
  -- an operand is strictly evaluated
  putInst $ MoveTo 0
  buildInsts x
  putInst Unwind
  putInst $ Primitive Neg
buildInsts (H.NthOf i x) = do
  -- an operand is strictly evaluated
  putInst $ MoveTo 0
  buildInsts x
  putInst Unwind
  putInst $ NthOf i
-- local let bindings
buildInsts (H.LocalLet a b) = withBoundExpr a $ buildInsts b
buildInsts H.LetBound = return ()
-- normal forms (no reduction is required)
buildInsts (H.Integer _) = return ()
buildInsts (H.FunctionRef _) = return ()
buildInsts (H.Parameter _) = return ()
