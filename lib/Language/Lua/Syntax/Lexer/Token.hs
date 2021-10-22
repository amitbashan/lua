module Language.Lua.Syntax.Lexer.Token where

import Language.Lua.Syntax.Lexer.Lexeme

data Position = Position Int Int Int

data Token = Token Lexeme Position String
