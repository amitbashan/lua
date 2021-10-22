{-# LANGUAGE TypeApplications #-}

module Main where

import CommandLine
import Control.Exception
import Control.Lens
import Control.Monad.State
import Language.Lua.Bytecode.Instruction as Instruction
import Language.Lua.Bytecode.Instruction.Argument as Argument
import Language.Lua.Compiler
import Language.Lua.Compiler.Environment as Environment
import Language.Lua.Compiler.Function as Function
import Language.Lua.Syntax
import Language.Lua.Syntax.Parser
import Options.Applicative
import System.Exit

main :: IO ()
main = do
  Options input output <- execParser optionsParserInfo
  sourceCode <-
    catch @IOError
      (readFile input)
      ((>> exitFailure) . print)

  case parse sourceCode of
    Right block ->
      print $
        execState
          (compile @FunctionSkeleton @() block Nothing)
          (Environment.new Nothing Nothing)
    Left syntaxError -> error syntaxError
