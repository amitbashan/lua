{-# LANGUAGE TemplateHaskell #-}

module Language.Lua.Compiler.Environment where

import Control.Lens
import Data.Maybe
import qualified Language.Lua.Bytecode.Instruction.Argument as Argument
import Language.Lua.Compiler.Function

data Environment = Environment
  { _function :: Function,
    _parent :: Maybe Function,
    _lastAllocatedRegister :: Maybe Argument.Register,
    _currentLoopBlockSize :: Maybe Int,
    _savedReturnValues :: Maybe Int,
    _usedUpvalues :: Bool
  }
  deriving (Show)

makeLenses ''Environment

new :: Maybe Function -> Maybe Function -> Environment
new function parent =
  Environment
    (fromMaybe empty function)
    parent
    Nothing
    Nothing
    Nothing
    False
