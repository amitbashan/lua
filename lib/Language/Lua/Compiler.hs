{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Lua.Compiler where

import Language.Lua.Compiler.Context
import Language.Lua.Compiler.Monad

class Compile a b where
  compile :: a -> Maybe (Context a) -> CompileMonad a b
