module Operators where

data BinaryOp
  = Add
  | Mul
  deriving (Show, Eq)

data SingleOp
  = Positive
  | Negative
  deriving (Show, Eq)
