module Language.Lua.Syntax.Operation.Unary where

data Unary
  = Negate
  | Not
  | Length
  deriving (Show, Eq)
