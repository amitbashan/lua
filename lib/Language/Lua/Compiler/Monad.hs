module Language.Lua.Compiler.Monad where

import Control.Monad.State
import Language.Lua.Compiler.Environment

type CompileMonad a = State Environment
