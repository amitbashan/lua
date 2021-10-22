{

module Language.Lua.Syntax.Parser where

import Data.List
import qualified Data.Map as Map
import qualified Data.Sequence as Sequence
import qualified Data.Text as Text
import qualified Language.Lua.Syntax as Syntax
import Language.Lua.Syntax.Lexer
import Language.Lua.Syntax.Lexer.Lexeme
import Language.Lua.Syntax.Lexer.Token
import qualified Language.Lua.Syntax.Operation.Binary as Operation.Binary
import qualified Language.Lua.Syntax.Operation.Unary as Operation.Unary

}

%name             parseTokens
%error            { parseError }
%lexer            { (alexMonadScan >>=) } { Token EOF _ _ }
%monad            { Alex }
%tokentype        { Token }
%errorhandlertype explist

%token
  and        { Token And _ _ }
  break      { Token Break _ _ }
  do         { Token Do _ _ }
  else       { Token Else _ _ }
  elseif     { Token ElseIf _ _ }
  end        { Token End _ _ }
  for        { Token For _ _ }
  function   { Token Function _ _ }
  if         { Token If _ _ }
  in         { Token In _ _ }
  local      { Token Local _ _ }
  nil        { Token Nil _ _ }
  not        { Token Not _ _ }
  or         { Token Or _ _ }
  repeat     { Token Repeat _ _ }
  return     { Token Return _ _ }
  then       { Token Then _ _ }
  until      { Token Until _ _ }
  while      { Token While _ _ }
  "+"        { Token Plus _ _ }
  "-"        { Token Minus _ _ }
  "*"        { Token Asterisk _ _ }
  "/"        { Token Slash _ _ }
  "%"        { Token Percent _ _ }
  "^"        { Token Caret _ _ }
  "#"        { Token Hash _ _ }
  "=="       { Token Equal _ _ }
  "~="       { Token NotEqual _ _ }
  "<"        { Token LessThan _ _ }
  ">"        { Token GreaterThan _ _ }
  "<="       { Token LessThanOrEqual _ _ }
  ">="       { Token GreaterThanOrEqual _ _ }
  "="        { Token Assign _ _ }
  "("        { Token LeftParentheses _ _ }
  ")"        { Token RightParentheses _ _ }
  "{"        { Token LeftBrace _ _ }
  "}"        { Token RightBrace _ _ }
  "["        { Token LeftBracket _ _ }
  "]"        { Token RightBracket _ _ }
  ";"        { Token Semicolon _ _ }
  ":"        { Token Colon _ _ }
  ","        { Token Comma _ _ }
  "."        { Token Dot _ _ }
  ".."       { Token Concatenate _ _ }
  "..."      { Token Ellipsis _ _ }
  IDENTIFIER { Token (Identifier $$) _ _ }
  BOOLEAN    { Token (Boolean $$) _ _ }
  NUMBER     { Token (Number $$) _ _ }
  STRING     { Token (String $$) _ _ }

%nonassoc "==" "~=" "<" ">" "<=" ">="
%left     "+" "-" "*" "/" "%"

%%

Block : {- Empty -}                         { Syntax.Block Sequence.empty Nothing Map.empty }
      | Statements                          { Syntax.Block $1 Nothing Map.empty }
      | Statements ControlTransferStatement { Syntax.Block $1 (Just $2) Map.empty }

-- Expressions

Expression : nil                     { Syntax.Nil }
           | BOOLEAN                 { Syntax.Boolean $1 }
           | NUMBER                  { Syntax.Number $1 }
           | STRING                  { Syntax.String $1 }
           | Table                   { $1 }
           | Identifier              { $1 }
           | Index                   { $1 }
           | Unary                   { $1 }
           | Binary                  { $1 }
           | Call                    { Syntax.CallExpression $1 }
           | Closure                 { $1 }
           | Ellipsis                { $1 }
           | ParenthesizedExpression { $1 }

ParenthesizedExpression : "(" Expression ")" { $2 }

ZeroOrMoreExpressions : {- Empty -}                          { Sequence.empty }
                      | Expression                           { Sequence.singleton $1 }
                      | ZeroOrMoreExpressions "," Expression { $1 Sequence.|> $3 }

OneOrMoreExpressions : Expression                          { Sequence.singleton $1 }
                     | OneOrMoreExpressions "," Expression { $1 Sequence.|> $3 }

-- Table Expression

Table : "{" TableItems "}" { Syntax.Table $2 }

TableItem : Expression                          { Syntax.TableItemExpression $1 }
          | "[" Expression "]" AssignExpression { Syntax.TableItemKeyValue $2 $4 }
          | IDENTIFIER AssignExpression         { Syntax.TableItemKeyValue (Syntax.Identifier $1) $2 }

TableItems : {- Empty -}                              { Sequence.empty }
           | TableItem                                { Sequence.singleton $1 }
           | TableItems TableItemsSeparator TableItem { $1 Sequence.|> $3 }

TableItemsSeparator : "," { $1 }
                    | ";" { $1 }

-- Identifier Expression

Identifier : IDENTIFIER { Syntax.Identifier $1 }

-- Index Expression

Index : IdentifierOrParenthesizedExpression IndexSuffixes { Syntax.Index $1 $2 }

IndexSuffix : "." Identifier     { $2 }
            | "[" Expression "]" { $2 }

IndexSuffixes : IndexSuffix               { Sequence.singleton $1 }
              | IndexSuffixes IndexSuffix { $1 Sequence.|> $2 }

-- Unary Expression

Unary : "-" Expression { Syntax.Unary Operation.Unary.Negate $2 }
      | not Expression { Syntax.Unary Operation.Unary.Not $2 }
      | "#" Expression { Syntax.Unary Operation.Unary.Length $2 }

-- Binary Expression

Binary : Expression "+" Expression  { Syntax.Binary (Operation.Binary.Arithmetic Operation.Binary.Add) $1 $3 }
       | Expression "-" Expression  { Syntax.Binary (Operation.Binary.Arithmetic Operation.Binary.Subtract) $1 $3 }
       | Expression "*" Expression  { Syntax.Binary (Operation.Binary.Arithmetic Operation.Binary.Multiply) $1 $3 }
       | Expression "/" Expression  { Syntax.Binary (Operation.Binary.Arithmetic Operation.Binary.Divide) $1 $3 }
       | Expression "%" Expression  { Syntax.Binary (Operation.Binary.Arithmetic Operation.Binary.Modulo) $1 $3 }
       | Expression "^" Expression  { Syntax.Binary (Operation.Binary.Arithmetic Operation.Binary.Power) $1 $3 }
       | Expression "==" Expression { Syntax.Binary (Operation.Binary.Relational Operation.Binary.Equal) $1 $3 }
       | Expression "~=" Expression { Syntax.Binary (Operation.Binary.Relational Operation.Binary.NotEqual) $1 $3 }
       | Expression "<" Expression  { Syntax.Binary (Operation.Binary.Relational Operation.Binary.LessThan) $1 $3 }
       | Expression ">" Expression  { Syntax.Binary (Operation.Binary.Relational Operation.Binary.GreaterThan) $1 $3 }
       | Expression "<=" Expression { Syntax.Binary (Operation.Binary.Relational Operation.Binary.LessThanOrEqual) $1 $3 }
       | Expression ">=" Expression { Syntax.Binary (Operation.Binary.Relational Operation.Binary.GreaterThanOrEqual) $1 $3 }
       | Expression and Expression  { Syntax.Binary (Operation.Binary.Logical Operation.Binary.And) $1 $3 }
       | Expression or Expression   { Syntax.Binary (Operation.Binary.Logical Operation.Binary.Or) $1 $3 }
       | Expression ".." Expression { Syntax.Binary (Operation.Binary.Miscellaneous Operation.Binary.Concatenate) $1 $3 }

-- Table Field

TableField : IdentifierOrParenthesizedExpression IndexSuffix               { Syntax.TableField $1 Sequence.empty $2 }
           | IdentifierOrParenthesizedExpression IndexSuffixes IndexSuffix { Syntax.TableField $1 $2 $3 }

-- Call Expression

Call : CallObject CallSuffix                { Syntax.RegularCall $1 $2 }
     | CallObject ":" IDENTIFIER CallSuffix { Syntax.MethodCall $1 $3 $4 }

CallObject : IdentifierOrParenthesizedExpression { $1 }
           | Index                               { $1 }
           | Call                                { Syntax.CallExpression $1 }

CallSuffix : "(" ZeroOrMoreExpressions ")" { $2 }
           | STRING                        { Sequence.singleton $ Syntax.String $1 }
           | Table                         { Sequence.singleton $1 }

-- Closure Expression

Closure : function FunctionSkeleton { Syntax.Closure $2 }

-- Ellipsis Expression

Ellipsis : "..." { Syntax.Ellipsis }

-- Statements

Statement : Declaration      { Syntax.Declaration $1 }
          | Assignment       { $1 }
          | If               { $1 }
          | Do               { $1 }
          | While            { $1 }
          | RepeatUntil      { $1 }
          | NumericFor       { $1 }
          | GenericFor       { $1 }
          | Call             { Syntax.CallStatement $1 }

Statements : {- Empty -}              { Sequence.empty }
           | Statement                { Sequence.singleton $1 }
           | Statements Statement     { $1 Sequence.|> $2 }
           | Statements ";" Statement { $1 Sequence.|> $3 }

-- Control Transfer Statements

ControlTransferStatement : return ZeroOrMoreExpressions { Syntax.Return $2 }
                         | break                        { Syntax.Break }

LoopControlTransferStatement : break { Syntax.Break }

-- Assignment Statement

Assignment : IdentifierOrTableFieldSequence AssignOneOrMoreExpressions { Syntax.Assignment $1 $2 }

-- If Statement

If : if Expression then Block ElseIfClauses ElseClause end { Syntax.If (Syntax.Condition $2 $4 Sequence.<| $5) $6 }

ElseIfClauses : {- Empty -}                { Sequence.empty }
              | ElseIfClause               { Sequence.singleton $1 }
              | ElseIfClauses ElseIfClause { $1 Sequence.|> $2 }

ElseIfClause : elseif Expression then Block { Syntax.Condition $2 $4 }

ElseClause : {- Empty -} { Nothing }
           | else Block  { Just $2 }

-- Do Block Statement

Do : DoBlock { Syntax.Do $1 }

-- While Statement

While : while Expression DoBlock { Syntax.While $ Syntax.Condition $2 $3 }

-- Repeat Until Statement

RepeatUntil : repeat Block until Expression { Syntax.RepeatUntil $ Syntax.Condition $4 $2 }

-- Numeric For Loop Statement

NumericFor : for IDENTIFIER AssignExpression "," Expression NumericForStep DoBlock { Syntax.NumericFor $2 $3 $5 $6 $7 }

NumericForStep : {- Empty -}    { Nothing }
               | "," Expression { Just $2 }

-- Generic For Loop Statement

GenericFor : for IDENTIFIERS in OneOrMoreExpressions DoBlock { Syntax.GenericFor $2 $4 $5 }

-- Declarations

Declaration : LocalVariables { $1 }
            | Function       { $1 }
            | LocalFunction  { $1 }
            | Method         { $1 }

-- Local Variables Declaration

LocalVariables : local Variables                            { Syntax.LocalVariables $2 Sequence.empty }
               | local Variables AssignOneOrMoreExpressions { Syntax.LocalVariables $2 $3 }

-- Function Declaration

Function : function Name FunctionSkeleton { Syntax.Function $2 $3 }

-- Local Function Declaration

LocalFunction : local function IDENTIFIER FunctionSkeleton { Syntax.LocalFunction $3 $4 }

-- Method Declaration

Method : function MethodName FunctionSkeleton { Syntax.Method $2 $3 }

MethodNameSuffix : ":" Identifier { $2 }

MethodName : Identifier MethodNameSuffix              { Right $ Syntax.TableField $1 Sequence.empty $2 }
           | Identifier NameSuffixes MethodNameSuffix { Right $ Syntax.TableField $1 $2 $3 }

-- Other

FunctionSkeleton : Parameters Block end { Syntax.FunctionSkeleton (fst $1) (snd $1) $2 }

-- Macros

AssignExpression : "=" Expression { $2 }

AssignOneOrMoreExpressions : "=" OneOrMoreExpressions { $2 }

IDENTIFIERS : IDENTIFIER                 { Sequence.singleton $1 }
            | IDENTIFIERS "," IDENTIFIER { $1 Sequence.|> $3 }

Name : IDENTIFIER                         { Left $1 }
     | Identifier NameSuffix              { Right $ Syntax.TableField $1 Sequence.empty $2 }
     | Identifier NameSuffixes NameSuffix { Right $ Syntax.TableField $1 $2 $3 }

NameSuffix : "." Identifier { $2 }

NameSuffixes : NameSuffix              { Sequence.singleton $1 }
             | NameSuffixes NameSuffix { $1 Sequence.|> $2 }

DoBlock : do Block end { $2 }

IdentifierOrParenthesizedExpression : Identifier              { $1 }
                                    | ParenthesizedExpression { $1 }

IdentifierOrTableField : IDENTIFIER { Left $1 }
                       | TableField { Right $1 }

IdentifierOrTableFieldSequence : IdentifierOrTableField                                    { Sequence.singleton $1 }
                               | IdentifierOrTableFieldSequence "," IdentifierOrTableField { $1 Sequence.|> $3 }

Variable : IDENTIFIER { $1 }

Variables : Variable               { Sequence.singleton $1 }
          | Variables "," Variable { $1 Sequence.|> $3 }

Parameters : "(" ")"                     { (Sequence.empty, False) }
           | "(" Variables ")"           { ($2, False) }
           | "(" Variables "," "..." ")" { ($2 Sequence.|> Text.pack "arg", True) }
           | "(" "..." ")"               { (Sequence.singleton $ Text.pack "arg", True) }

{

parse :: String -> Either String Syntax.FunctionSkeleton
parse = ((Syntax.FunctionSkeleton Sequence.empty True) <$>) . flip runAlex parseTokens

parseError :: (Token, [String]) -> Alex a
parseError (Token _ (Position _ line column) token, expectedTokens) =
  alexError
    (
        "Syntax error at "
        ++
          (if null token
            then "end of file"
            else
              show line
                ++ ", column "
                ++ show column
                ++ ", near `"
                ++ token
                ++ "."
           )
        ++ ". Expected: "
        ++ intercalate ", " expectedTokens
    )

}
