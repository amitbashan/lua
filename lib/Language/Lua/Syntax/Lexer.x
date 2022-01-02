{

module Language.Lua.Syntax.Lexer where

import Data.Char as Char
import qualified Data.Text as Text
import Language.Lua.Syntax.Lexer.Lexeme
import Language.Lua.Syntax.Lexer.Token

}

%wrapper "monad"

$decimalDigit = [0-9 _]

$hexadecimalDigit = [$decimalDigit a-f A-F]

$alphabet = [a-z A-Z]

$ascii = [\x00-\x21 \x23-\x26 \x28-\x7F]

$exponent = [e E]

$sign = [\+ \-]

@comment = "--".*

@identifier = [_ $alphabet][_ $alphabet $decimalDigit]*

@integer = $decimalDigit+

@hexadecimal = 0 [x X] $hexadecimalDigit+

@decimal = $decimalDigit+ (\. $decimalDigit+ ($exponent $sign? $decimalDigit+)?)

@number = @integer
        | @decimal
        | @hexadecimal

@signednumber = (\-)? @number

@string = \' $ascii+ \'
        | \" $ascii+ \"

tokens :-
  $white+       ;
  @comment      ;
  and           { makeToken $ const And }
  break         { makeToken $ const Break }
  do            { makeToken $ const Do }
  else          { makeToken $ const Else }
  elseif        { makeToken $ const ElseIf }
  end           { makeToken $ const End }
  false         { makeToken $ const $ Boolean False }
  for           { makeToken $ const For }
  function      { makeToken $ const Function }
  if            { makeToken $ const If }
  in            { makeToken $ const In }
  local         { makeToken $ const Local }
  nil           { makeToken $ const Nil }
  not           { makeToken $ const Not }
  or            { makeToken $ const Or }
  repeat        { makeToken $ const Repeat }
  return        { makeToken $ const Return }
  then          { makeToken $ const Then }
  true          { makeToken $ const $ Boolean True }
  until         { makeToken $ const Until }
  while         { makeToken $ const While }
  "+"           { makeToken $ const Plus }
  "-"           { makeToken $ const Minus }
  "*"           { makeToken $ const Asterisk }
  "/"           { makeToken $ const Slash }
  "%"           { makeToken $ const Percent }
  "^"           { makeToken $ const Caret }
  "#"           { makeToken $ const Hash }
  "=="          { makeToken $ const Equal }
  "~="          { makeToken $ const NotEqual }
  "<="          { makeToken $ const LessThanOrEqual }
  ">="          { makeToken $ const GreaterThanOrEqual }
  "<"           { makeToken $ const LessThan }
  ">"           { makeToken $ const GreaterThan }
  "="           { makeToken $ const Assign }
  "("           { makeToken $ const LeftParentheses }
  ")"           { makeToken $ const RightParentheses }
  "{"           { makeToken $ const LeftBrace }
  "}"           { makeToken $ const RightBrace }
  "["           { makeToken $ const LeftBracket }
  "]"           { makeToken $ const RightBracket }
  ";"           { makeToken $ const Semicolon }
  ":"           { makeToken $ const Colon }
  ","           { makeToken $ const Comma }
  "."           { makeToken $ const Dot }
  ".."          { makeToken $ const Concatenate }
  "..."         { makeToken $ const Ellipsis }
  @identifier   { makeToken $ Identifier . Text.pack }
  @signednumber { makeToken $ Number . read }
  @string       { makeToken $ String . Text.tail . Text.init . Text.pack }

{

alexEOF :: Alex Token
alexEOF = return $ Token EOF (Position 0 0 0) ""

makeToken :: (String -> Lexeme) -> AlexInput -> Int -> Alex Token
makeToken mapper ((AlexPn offset line column), _, _, string) length =
  let string' = take length string
   in return $
        Token
          (mapper string')
          (Position offset line column)
          string'

}
