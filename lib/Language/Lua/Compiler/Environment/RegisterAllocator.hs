module Language.Lua.Compiler.Environment.RegisterAllocator where

import Control.Lens
import Language.Lua.Bytecode.Instruction.Argument
import Language.Lua.Compiler.Environment
import Language.Lua.Compiler.Function
import Language.Lua.Compiler.Monad
import qualified Language.Lua.Compiler.RegisterAllocator as RegisterAllocator

allocate :: CompileMonad a Register
allocate = do
  register <-
    zoom
      (function . registerAllocator)
      RegisterAllocator.allocate

  lastAllocatedRegister ?= register

  return register

free :: Register -> CompileMonad a ()
free register =
  zoom
    (function . registerAllocator)
    (RegisterAllocator.free register)
