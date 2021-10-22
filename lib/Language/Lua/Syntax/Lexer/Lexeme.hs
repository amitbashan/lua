module Language.Lua.Syntax.Lexer.Lexeme where

import Data.Text

data Lexeme
  = And
  | Break
  | Do
  | Else
  | ElseIf
  | End
  | For
  | Function
  | If
  | In
  | Local
  | Nil
  | Not
  | Or
  | Repeat
  | Return
  | Then
  | Until
  | While
  | Plus
  | Minus
  | Asterisk
  | Slash
  | Percent
  | Caret
  | Hash
  | Equal
  | NotEqual
  | LessThanOrEqual
  | GreaterThanOrEqual
  | LessThan
  | GreaterThan
  | Assign
  | LeftParentheses
  | RightParentheses
  | LeftBrace
  | RightBrace
  | LeftBracket
  | RightBracket
  | Semicolon
  | Colon
  | Comma
  | Dot
  | Concatenate
  | Ellipsis
  | Identifier Text
  | Boolean Bool
  | Number Double
  | String Text
  | EOF
