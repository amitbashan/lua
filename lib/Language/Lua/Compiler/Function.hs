{-# LANGUAGE TemplateHaskell #-}

module Language.Lua.Compiler.Function where

import Control.Lens
import qualified Data.Map as Map
import qualified Data.Sequence as Sequence
import qualified Data.Text as Text
import Language.Lua.Bytecode.Instruction as Instruction
import qualified Language.Lua.Bytecode.Instruction.Argument as Argument
import qualified Language.Lua.Compiler.Configuration.Limit as Configuration.Limit
import Language.Lua.Compiler.RegisterAllocator
import Language.Lua.Compiler.Value

data Function = Function
  { _registerAllocator :: RegisterAllocator,
    _variadic :: Bool,
    _symbolMap :: Map.Map Text.Text Argument.Register,
    _code :: Sequence.Seq Instruction,
    _constants :: Map.Map Value Int,
    _closures :: Sequence.Seq Function
  }
  deriving (Show)

makeLenses ''Function

empty :: Function
empty =
  Function
    (new Configuration.Limit.maximumStackFrameSize)
    False
    Map.empty
    Sequence.empty
    Map.empty
    Sequence.empty
