{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Lua.Bytecode.Instruction.Argument where

newtype Register = Register Int
  deriving (Show, Eq, Num)

newtype Constant = Constant Int
  deriving (Show)

newtype RegisterOrConstant = RegisterOrConstant {inner :: Either Register Constant}
  deriving (Show)

newtype Function = Function Int
  deriving (Show)
