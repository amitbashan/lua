module Language.Lua.Bytecode.Instruction where

import Language.Lua.Bytecode.Instruction.Argument

data Instruction
  = Move Register Register
  | LoadConstant Register Constant
  | LoadBoolean Register Bool Bool
  | LoadNil Register Register
  | GetUpvalue Register Register
  | GetGlobal Register Constant
  | GetTable Register Register RegisterOrConstant
  | SetGlobal Register Constant
  | SetUpvalue Register Register
  | SetTable Register RegisterOrConstant RegisterOrConstant
  | NewTable Register Int Int
  | Self Register Register RegisterOrConstant
  | Add Register RegisterOrConstant RegisterOrConstant
  | Subtract Register RegisterOrConstant RegisterOrConstant
  | Divide Register RegisterOrConstant RegisterOrConstant
  | Multiply Register RegisterOrConstant RegisterOrConstant
  | Modulo Register RegisterOrConstant RegisterOrConstant
  | Power Register RegisterOrConstant RegisterOrConstant
  | Negate Register Register
  | Not Register Register
  | Length Register Register
  | Concatenate Register Register Register
  | Jump Int
  | Equal Bool RegisterOrConstant RegisterOrConstant
  | LessThan Bool RegisterOrConstant RegisterOrConstant
  | LessThanOrEqual Bool RegisterOrConstant RegisterOrConstant
  | Test Register Bool
  | TestSet Register Register Bool
  | Call Register Int Int
  | TailCall Register Int
  | Return Register Int
  | NumericForLoop Register Int
  | PrepareNumericForLoop Register Int
  | GenericForLoop Register Int
  | SetList Register Int Int
  | Close Register
  | Closure Register Function
  | Ellipsis Register Register
  deriving (Show)
