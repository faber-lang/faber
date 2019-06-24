module Operators where

data BinaryOp
  = Add
  | Mul
  | Sub
  | Eq
  deriving (Show, Eq)

data SingleOp
  = Positive
  | Negative
  deriving (Show, Eq)
