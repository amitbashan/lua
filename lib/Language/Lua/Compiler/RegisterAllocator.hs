{-# LANGUAGE TemplateHaskell #-}

module Language.Lua.Compiler.RegisterAllocator where

import Control.Exception
import Control.Lens
import Control.Monad.State
import Data.Sequence as Sequence
import Language.Lua.Bytecode.Instruction.Argument (Register (..))
import Language.Lua.Compiler.RegisterAllocator.Error

newtype RegisterAllocator = RegisterAllocator {_registers :: Seq Bool}
  deriving (Show)

makeLenses ''RegisterAllocator

new :: Int -> RegisterAllocator
new length = RegisterAllocator $ Sequence.replicate length False

allocate :: State RegisterAllocator Register
allocate = do
  registerAllocator <- gets _registers

  case elemIndexL False registerAllocator of
    Just index -> do
      registers %= update index True
      return $ Register $ fromIntegral index
    Nothing -> throw OutOfRegisters

free :: Register -> State RegisterAllocator ()
free (Register register) = do
  registers
    %= update
      (fromIntegral register)
      False
