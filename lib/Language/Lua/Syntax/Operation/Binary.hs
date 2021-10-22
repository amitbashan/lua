module Language.Lua.Syntax.Operation.Binary where

data Binary
  = Arithmetic Arithmetic
  | Relational Relational
  | Logical Logical
  | Miscellaneous Miscellaneous
  deriving (Show, Eq)

data Arithmetic
  = Add
  | Subtract
  | Multiply
  | Divide
  | Modulo
  | Power
  deriving (Show, Eq)

data Relational
  = Equal
  | NotEqual
  | LessThan
  | GreaterThan
  | LessThanOrEqual
  | GreaterThanOrEqual
  deriving (Show, Eq)

data Logical
  = And
  | Or
  deriving (Show, Eq)

data Miscellaneous
  = Concatenate
  deriving (Show, Eq)
