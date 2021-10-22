module Language.Lua.Compiler.Environment.Function.ConstantTable where

import Control.Lens
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe
import Language.Lua.Bytecode.Instruction.Argument
import Language.Lua.Compiler.Environment
import Language.Lua.Compiler.Function
import Language.Lua.Compiler.Monad
import Language.Lua.Compiler.Value

insert :: Value -> CompileMonad a Constant
insert value = do
  map <- gets (^. function . constants)

  let result = Map.lookup value map
      index = fromMaybe (Map.size map) result

  when (isNothing result) $
    function . constants %= Map.insert value index

  return $ Constant index
