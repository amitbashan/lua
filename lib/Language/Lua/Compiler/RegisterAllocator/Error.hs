module Language.Lua.Compiler.RegisterAllocator.Error where

import Control.Exception

data Error
  = OutOfRegisters
  deriving (Show)

instance Exception Error
