module Language.Lua.Compiler.Value where

import Data.Text
import Language.Lua.Compiler.Configuration.Size

data Value
  = Nil
  | Boolean Bool
  | Number Number
  | String Text
  deriving (Show, Eq, Ord)
