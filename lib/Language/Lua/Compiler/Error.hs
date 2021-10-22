module Language.Lua.Compiler.Error where

import Control.Exception

data Error
  = EllipsisInNonVariadicFunction
  | BreakStatementOutsideLoop
  deriving (Show)

instance Exception Error
