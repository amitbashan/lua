{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Lua.Syntax where

import Control.Exception
import Control.Lens hiding (Context)
import Control.Monad.State
import Data.Foldable
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Sequence as Sequence
import qualified Data.Text as Text
import qualified Language.Lua.Bytecode.Instruction as Instruction
import qualified Language.Lua.Bytecode.Instruction.Argument as Argument
import Language.Lua.Compiler
import qualified Language.Lua.Compiler.Configuration.Limit as Configuration.Limit
import Language.Lua.Compiler.Configuration.Size
import Language.Lua.Compiler.Context
import Language.Lua.Compiler.Environment as Environment
import qualified Language.Lua.Compiler.Environment.Function.ConstantTable as ConstantTable
import Language.Lua.Compiler.Environment.RegisterAllocator
import qualified Language.Lua.Compiler.Error as Error
import Language.Lua.Compiler.Function as Function hiding (Function)
import qualified Language.Lua.Compiler.Value as Value
import qualified Language.Lua.Syntax.Operation.Binary as Operation.Binary
import qualified Language.Lua.Syntax.Operation.Unary as Operation.Unary

mapMWithIndex :: Applicative f => (Int -> a -> f b) -> Sequence.Seq a -> f (Sequence.Seq b)
mapMWithIndex f = sequenceA . Sequence.mapWithIndex f

mapMWithIndex_ :: Applicative f => (Int -> a -> f b) -> Sequence.Seq a -> f ()
mapMWithIndex_ f = void . mapMWithIndex f

head' :: Sequence.Seq a -> Maybe a
head' Sequence.Empty = Nothing
head' (a Sequence.:<| _) = Just a

last' :: Sequence.Seq a -> Maybe a
last' Sequence.Empty = Nothing
last' (_ Sequence.:|> a) = Just a

init' :: Sequence.Seq a -> Sequence.Seq a
init' Sequence.Empty = Sequence.empty
init' (a Sequence.:|> _) = a

padRightAndZip :: b -> Sequence.Seq a -> Sequence.Seq b -> (Sequence.Seq (a, b), Int)
padRightAndZip b as bs
  | l <= 0 = (Sequence.zip as bs, 0)
  | otherwise = (Sequence.zip as $ bs <> Sequence.replicate l b, l)
  where
    l = Sequence.length as - Sequence.length bs

type Identifier = Text.Text

type Name = Either Identifier TableField

type Parameter = Identifier

data Block = Block (Sequence.Seq Statement) (Maybe ControlTransferStatement) (Map.Map Text.Text Expression)
  deriving (Show, Eq)

data Expression
  = Nil
  | Boolean Bool
  | Number Number
  | String Text.Text
  | Table (Sequence.Seq TableItem)
  | Identifier Text.Text
  | Index Expression (Sequence.Seq Expression)
  | Unary Operation.Unary.Unary Expression
  | Binary Operation.Binary.Binary Expression Expression
  | CallExpression Call
  | Closure FunctionSkeleton
  | Ellipsis
  deriving (Show, Eq)

data instance Context Expression = SkipNext | SkipMove | CopyAll
  deriving (Eq)

data FunctionSkeleton = FunctionSkeleton
  { _parameters :: Sequence.Seq Parameter,
    _variadic :: Bool,
    _body :: Block
  }
  deriving (Show, Eq)

data TableItem
  = TableItemExpression Expression
  | TableItemKeyValue Expression Expression
  deriving (Show, Eq)

newtype instance Context TableItem = PassTable Argument.Register

data TableField = TableField Expression (Sequence.Seq Expression) Expression
  deriving (Show, Eq)

data Call
  = RegularCall Expression (Sequence.Seq Expression)
  | MethodCall Expression Identifier (Sequence.Seq Expression)
  deriving (Show, Eq)

data instance Context Call = SavedReturnValues Int | TailCall
  deriving (Eq, Show)

data Statement
  = Declaration Declaration
  | Assignment (Sequence.Seq Name) (Sequence.Seq Expression)
  | If (Sequence.Seq Condition) (Maybe Block)
  | Do Block
  | While Condition
  | RepeatUntil Condition
  | NumericFor Identifier Expression Expression (Maybe Expression) Block
  | GenericFor (Sequence.Seq Identifier) (Sequence.Seq Expression) Block
  | CallStatement Call
  deriving (Show, Eq)

data Condition = Condition Expression Block
  deriving (Show, Eq)

data ControlTransferStatement
  = Break
  | Return (Sequence.Seq Expression)
  deriving (Show, Eq)

data Declaration
  = LocalVariables (Sequence.Seq Identifier) (Sequence.Seq Expression)
  | Function Name FunctionSkeleton
  | LocalFunction Identifier FunctionSkeleton
  | Method Name FunctionSkeleton
  deriving (Show, Eq)

newtype LoopBlockState a = LoopBlockState (State Environment a)

makeLenses ''FunctionSkeleton

instance Compile (LoopBlockState a) a where
  compile (LoopBlockState state) _ = do
    currentState <- get

    let (result, newState) =
          runState
            ( do
                currentLoopBlockSize ?= newCodeSize - 2

                (result, newState) <- get <&> runState state

                put newState
                currentLoopBlockSize .= Nothing
                return result
            )
            currentState
        newCodeSize = Sequence.length (newState ^. function . code)

    put newState

    return result

instance Compile Block Int where
  compile (Block statements controlTransferStatement _) _ = do
    originalCodeSize <-
      gets
        (^. function . code)
        <&> Sequence.length

    mapM_
      ( flip
          (compile @Statement @())
          Nothing
      )
      statements

    traverse_
      ( flip
          (compile @ControlTransferStatement @())
          Nothing
      )
      controlTransferStatement

    currentCodeSize <-
      gets
        (^. function . code)
        <&> Sequence.length

    return $ fromIntegral $ currentCodeSize - originalCodeSize

instance Compile Expression Argument.Register where
  compile Nil _ = do
    destination <- allocate

    function . code
      %= ( |>
             Instruction.LoadNil
               destination
               destination
         )

    return destination
  compile (Boolean value) context = do
    destination <- allocate

    function . code
      %= ( |>
             Instruction.LoadBoolean
               destination
               value
               (context == Just SkipNext)
         )

    return destination
  compile expression@(Number _) _ =
    compile
      @Expression
      @(Argument.Register, Maybe Argument.Constant)
      expression
      Nothing
      <&> (^. _1)
  compile expression@(String _) _ =
    compile
      @Expression
      @(Argument.Register, Maybe Argument.Constant)
      expression
      Nothing
      <&> (^. _1)
  compile (Table items) _ = do
    destination <- allocate

    if Sequence.null items
      then function . code %= (|> Instruction.NewTable destination 0 0)
      else
        mapMWithIndex_
          ( \index items -> do
              let (keyValues, values) =
                    Sequence.partition
                      ( \case
                          TableItemExpression _ -> False
                          _ -> True
                      )
                      items
                      & both %~ Sequence.length

              function
                . code
                %= ( |>
                       Instruction.NewTable
                         destination
                         values
                         keyValues
                   )

              mapM_
                ( flip
                    (compile @TableItem @Argument.Register)
                    (Just $ PassTable destination)
                )
                items

              function
                . code
                %= ( |>
                       Instruction.SetList
                         destination
                         values
                         (index + 1)
                   )
          )
          $ Sequence.chunksOf
            Configuration.Limit.fieldsPerFlush
            items

    return destination
  compile expression@(Identifier name) context = do
    map <- gets (^. function . symbolMap)

    case Map.lookup name map of
      Just register ->
        if Just SkipMove == context
          then return register
          else do
            destination <- allocate

            function . code
              %= ( |>
                     Instruction.Move
                       destination
                       register
                 )

            return destination
      _ -> do
        parent <- gets _parent

        case parent <&> (^. symbolMap) >>= Map.lookup name of
          Just register -> do
            destination <- allocate

            usedUpvalues .= True
            function . code
              %= ( |>
                     Instruction.GetUpvalue
                       destination
                       register
                 )

            return destination
          _ ->
            compile
              @Expression
              @(Argument.Register, Maybe Argument.Constant)
              expression
              Nothing
              <&> (^. _1)
  compile (Index object indices) _ = do
    destination <- compile object (Just SkipMove)

    mapM_
      ( \index -> do
          index <- compile index Nothing

          function . code
            %= ( |>
                   Instruction.GetTable
                     destination
                     destination
                     index
               )
      )
      indices

    return destination
  compile (Unary operation expression) _ = do
    destination <- compile expression Nothing

    function . code
      %= ( |>
             ( case operation of
                 Operation.Unary.Negate -> Instruction.Negate
                 Operation.Unary.Not -> Instruction.Not
                 Operation.Unary.Length -> Instruction.Length
             )
               destination
               destination
         )

    return destination
  compile (Binary (Operation.Binary.Arithmetic operation) left right) _ = do
    left <- compile left Nothing
    right <- compile right Nothing
    destination <-
      case Argument.inner left of
        Left register -> return register
        _ -> allocate

    function . code
      %= ( |>
             ( case operation of
                 Operation.Binary.Add -> Instruction.Add
                 Operation.Binary.Subtract -> Instruction.Subtract
                 Operation.Binary.Multiply -> Instruction.Multiply
                 Operation.Binary.Divide -> Instruction.Divide
                 Operation.Binary.Modulo -> Instruction.Modulo
                 Operation.Binary.Power -> Instruction.Power
             )
               destination
               left
               right
         )

    traverse_
      (free . Sequence.singleton)
      (Argument.inner right ^? _Left)

    return destination
  compile (Binary (Operation.Binary.Relational operation) left right) _ = do
    destination <- compile left Nothing
    right <- compile right Nothing

    function . code
      %= ( <>
             Sequence.fromList
               [ ( case operation of
                     Operation.Binary.LessThan -> Instruction.LessThan True
                     Operation.Binary.GreaterThan -> flip $ Instruction.LessThan True
                     Operation.Binary.LessThanOrEqual -> Instruction.LessThanOrEqual True
                     Operation.Binary.GreaterThanOrEqual -> flip $ Instruction.LessThanOrEqual True
                     Operation.Binary.Equal -> Instruction.Equal True
                     Operation.Binary.NotEqual -> Instruction.Equal False
                 )
                   (Argument.RegisterOrConstant $ Left destination)
                   right,
                 Instruction.Jump 1,
                 Instruction.LoadBoolean destination False True,
                 Instruction.LoadBoolean destination True False
               ]
         )

    return destination
  compile (Binary (Operation.Binary.Logical operation) left right) _ = do
    destination <- compile left Nothing

    function . code
      %= ( <>
             Sequence.fromList
               [ Instruction.Test
                   destination
                   ( case operation of
                       Operation.Binary.And -> False
                       Operation.Binary.Or -> True
                   ),
                 Instruction.Jump 1
               ]
         )

    free (Sequence.singleton destination)

    compile right Nothing
  compile (Binary (Operation.Binary.Miscellaneous Operation.Binary.Concatenate) left right) _ = do
    left <- compile left Nothing
    right <-
      mapM
        (`compile` Nothing)
        (unfold right)
        <&> fromJust . last'

    function . code
      %= ( |>
             Instruction.Concatenate
               left
               left
               (right + 1)
         )

    free (Sequence.singleton right)

    return left
    where
      unfold =
        Sequence.unfoldr
          ( \case
              Just (Binary (Operation.Binary.Miscellaneous Operation.Binary.Concatenate) left right) -> Just (left, Just right)
              Just other -> Just (other, Nothing)
              _ -> Nothing
          )
          . Just
  compile (CallExpression call) _ = compile call Nothing
  compile (Closure functionSkeleton) _ = do
    currentFunction <- gets _function
    destination <- allocate

    let environment =
          execState
            ( compile
                @FunctionSkeleton
                @()
                functionSkeleton
                Nothing
            )
            ( Environment.new
                Nothing
                (Just currentFunction)
            )

    usedUpvalues .= environment ^. usedUpvalues
    function . closures %= (|> environment ^. function)
    function . code
      %= ( |>
             Instruction.Closure
               destination
               ( Argument.Function $
                   Sequence.length $
                     environment ^. function . closures
               )
         )

    return destination
  compile Ellipsis context = do
    variadic <- gets (^. function . Function.variadic)

    if variadic
      then do
        destination <- allocate

        function . code
          %= ( |>
                 Instruction.Ellipsis
                   destination
                   ( if Just CopyAll == context
                       then 0
                       else destination + 2
                   )
             )

        return destination
      else throw Error.EllipsisInNonVariadicFunction

instance Compile Expression Argument.RegisterOrConstant where
  compile expression _ =
    compile
      expression
      Nothing
      >>= \case
        Just value ->
          ConstantTable.insert value
            <&> Right
        _ ->
          compile
            @Expression
            @(Argument.Register, Maybe Argument.Constant)
            expression
            Nothing
            <&> Left . fst
      <&> Argument.RegisterOrConstant

instance Compile Expression (Maybe Value.Value) where
  compile expression _ =
    compile
      @Expression
      @(Maybe (Value.Value, Argument.Register -> Argument.Constant -> Instruction.Instruction))
      expression
      Nothing
      <&> (^? _Just . _1)

instance Compile Expression (Maybe (Value.Value, Argument.Register -> Argument.Constant -> Instruction.Instruction)) where
  compile expression _ = do
    return $ case expression of
      Boolean value -> Just (Value.Boolean value, Instruction.LoadConstant)
      Number value -> Just (Value.Number value, Instruction.LoadConstant)
      String value -> Just (Value.String value, Instruction.LoadConstant)
      Identifier name -> Just (Value.String name, Instruction.GetGlobal)
      _ -> Nothing

instance Compile Expression (Argument.Register, Maybe Argument.Constant) where
  compile expression _ = do
    result <-
      compile
        @Expression
        @(Maybe (Value.Value, Argument.Register -> Argument.Constant -> Instruction.Instruction))
        expression
        Nothing

    case result of
      Just (value, instruction) -> do
        destination <- allocate
        constant <- ConstantTable.insert value

        function . code
          %= ( |>
                 instruction
                   destination
                   constant
             )

        return (destination, Just constant)
      _ -> compile expression Nothing <&> (,Nothing)

instance Compile FunctionSkeleton () where
  compile (FunctionSkeleton parameters variadic body@(Block _ controlTransferStatement _)) _ = do
    function . Function.variadic .= variadic

    mapM_
      ( \parameter ->
          allocate
            >>= (function . symbolMap %=) . Map.insert parameter
      )
      parameters

    compile @Block @Int body Nothing

    when (isNothing controlTransferStatement) $
      compile (Return Sequence.empty) Nothing

instance Compile TableItem Argument.Register where
  compile (TableItemExpression value) _ = compile value Nothing
  compile (TableItemKeyValue key value) (Just (PassTable table)) = do
    key <- compile key Nothing
    value <- compile value Nothing

    function . code
      %= ( |>
             Instruction.SetTable
               table
               key
               value
         )

    return table

instance Compile Call Argument.Register where
  compile call context = do
    Environment.savedReturnValues ?= savedReturnValues

    case call of
      RegularCall object arguments -> do
        object <- compile object Nothing
        argumentsRegisters <- compile arguments Nothing

        function . code
          %= ( |>
                 ( if context == Just TailCall
                     then flip . const . Instruction.TailCall
                     else Instruction.Call
                 )
                   object
                   (calculateCallArguments False arguments)
                   savedReturnValues
             )

        free argumentsRegisters

        return object
      MethodCall object method arguments -> do
        object <- compile object Nothing
        method <- compile (Identifier method) Nothing

        function . code
          %= ( |>
                 Instruction.Self
                   object
                   object
                   method
             )

        argumentsRegisters <- compile arguments Nothing

        function . code
          %= ( |>
                 Instruction.Call
                   object
                   (calculateCallArguments True arguments)
                   savedReturnValues
             )

        free argumentsRegisters

        return object
    where
      calculateCallArguments isMethodCall arguments =
        case last' arguments of
          Just Ellipsis -> 0
          Just (CallExpression _) -> 0
          _ -> Sequence.length arguments + fromEnum isMethodCall + 1
      savedReturnValues =
        case context of
          Just (SavedReturnValues values) -> values + 1
          _ -> 1

instance Compile (Sequence.Seq Expression) (Sequence.Seq Argument.Register) where
  compile arguments _ =
    mapMWithIndex
      ( \index argument ->
          case argument of
            CallExpression call ->
              compile
                call
                ( Just $
                    SavedReturnValues $
                      fromEnum $ index + 1 /= Sequence.length arguments
                )
            other ->
              compile
                @Expression
                other
                (Just CopyAll)
      )
      arguments

instance Compile Statement () where
  compile (Declaration declaration) _ =
    void $
      compile
        @Declaration
        @(Sequence.Seq Argument.Register)
        declaration
        Nothing
  compile (Assignment names expressions) _ =
    let (pairs, leftovers) = padRightAndZip Nothing names (Just <$> expressions)
     in mapMWithIndex_
          ( \index pair ->
              let savedReturnValues =
                    Just $
                      SavedReturnValues $
                        if index + 1 == Sequence.length pairs - leftovers
                          then leftovers + 1
                          else 1
               in case pair of
                    (Left name, expression) -> do
                      map <- gets (^. function . symbolMap)

                      case Map.lookup name map of
                        Just register -> do
                          traverse
                            ( \expression -> do
                                free (Sequence.singleton register)

                                compile
                                  @Expression
                                  @Argument.Register
                                  expression
                                  Nothing
                            )
                            expression
                        _ -> do
                          parent <- gets _parent

                          case parent <&> (^. symbolMap) >>= Map.lookup name of
                            Just register -> do
                              destination <-
                                traverse
                                  ( \expression -> do
                                      destination <-
                                        compile
                                          @Expression
                                          @Argument.Register
                                          expression
                                          Nothing

                                      function . code
                                        %= ( |>
                                               Instruction.SetUpvalue
                                                 destination
                                                 register
                                           )

                                      return destination
                                  )
                                  expression

                              return $ Just $ fromMaybe register destination
                            _ -> do
                              register <- compileAssignedValue expression savedReturnValues
                              constant <- ConstantTable.insert (Value.String name)

                              function . code
                                %= ( |>
                                       Instruction.SetGlobal
                                         register
                                         constant
                                   )

                              return $ Just register
                    (Right (TableField object indices field), expression) -> do
                      object <- compile object (Just SkipMove)
                      field <- compile field Nothing

                      mapM_
                        ( \index -> do
                            index <-
                              compile
                                @Expression
                                @Argument.RegisterOrConstant
                                index
                                Nothing

                            function . code
                              %= ( |>
                                     Instruction.GetTable
                                       object
                                       object
                                       index
                                 )
                        )
                        indices

                      register <-
                        compileAssignedValue
                          expression
                          savedReturnValues
                          <&> Argument.RegisterOrConstant . Left

                      function . code
                        %= ( |>
                               Instruction.SetTable
                                 object
                                 field
                                 register
                           )

                      free (Sequence.singleton object)

                      return $ Argument.inner register ^? _Left
                    >>= traverse_
                      (free . Sequence.singleton)
          )
          pairs
          <* (savedReturnValues .= Nothing)
    where
      compileAssignedValue expression savedReturnValues =
        case expression of
          Just (CallExpression call) -> compile call savedReturnValues
          Just expression -> compile expression Nothing
          _ -> do
            savedReturnValues <- gets _savedReturnValues

            case savedReturnValues of
              Just savedReturnValues -> do
                let next = savedReturnValues - 1

                Environment.savedReturnValues
                  .= if savedReturnValues == 1
                    then Nothing
                    else Just next

                return $ Argument.Register next
              _ -> allocate
  compile (If conditions maybeElseBlock) _ = do
    indices <-
      mapMWithIndex
        ( \index condition ->
            compile
              condition
              Nothing
              <&> (+) (index * 2 + 1)
        )
        conditions
    blockSize <-
      gets (^. function . code)
        <&> (+) (Sequence.length indices * 2) . Sequence.length

    mapM_
      ( \blockIndex ->
          function . code
            %= Sequence.insertAt
              blockIndex
              (Instruction.Jump $ blockSize - blockIndex)
      )
      (init' indices)

    traverse_
      ( \elseBlock -> do
          conditionIndex <-
            gets (^. function . code)
              <&> Sequence.length
          size <- compile elseBlock Nothing

          function . code
            %= Sequence.insertAt
              conditionIndex
              (Instruction.Jump size)
      )
      maybeElseBlock
  compile (Do body) _ = do
    lastAllocatedRegister <- gets _lastAllocatedRegister <&> fromJust
    oldRegisterAllocator <- gets (^. function . registerAllocator)
    oldSymbolMap <- gets (^. function . symbolMap)

    compile @Block @Int body Nothing

    function . registerAllocator .= oldRegisterAllocator
    function . symbolMap .= oldSymbolMap

    gets _usedUpvalues
      >>= flip
        when
        ( do
            usedUpvalues .= False
            function . code
              %= ( |>
                     Instruction.Close lastAllocatedRegister
                 )
        )
  compile (While condition) _ = do
    codeSize <-
      gets (^. function . code)
        <&> Sequence.length

    compile
      @(LoopBlockState ())
      @()
      ( LoopBlockState $
          void $
            compile
              @Condition
              @Int
              condition
              Nothing
      )
      Nothing

    newCodeSize <-
      gets (^. function . code)
        <&> Sequence.length

    function . code
      %= ( |>
             Instruction.Jump (codeSize - newCodeSize - 1)
         )
  compile (RepeatUntil (Condition expression body)) _ =
    compile
      ( While
          ( Condition
              (Unary Operation.Unary.Not expression)
              body
          )
      )
      Nothing
  compile (NumericFor variable start end step body) _ = do
    compile
      ( LoopBlockState $ do
          start <- compile start Nothing
          end <- compile end Nothing
          step <-
            compile
              (fromMaybe (Number 1) step)
              Nothing

          compile
            @Declaration
            @(Sequence.Seq Argument.Register)
            ( LocalVariables
                (Sequence.singleton variable)
                Sequence.empty
            )
            Nothing

          prepareInstructionIndex <-
            gets
              (^. function . code)
              <&> Sequence.length

          bodySize <-
            compile
              (Block (Sequence.singleton $ Do body) Nothing Map.empty)
              Nothing

          function . code
            %= Sequence.insertAt
              prepareInstructionIndex
              ( Instruction.PrepareNumericForLoop
                  start
                  bodySize
              )

          function . code
            %= ( |>
                   Instruction.NumericForLoop
                     start
                     (- bodySize - 1)
               )

          function . symbolMap %= Map.delete variable

          free $
            Sequence.fromList
              [start, end, step, step + 1]
      )
      Nothing
  compile (GenericFor variables expressions body) _ = do
    bodySize <-
      compile
        @(LoopBlockState Int)
        @Int
        ( LoopBlockState $ do
            expressions <-
              compile (Sequence.take 3 expressions) Nothing
            variables <-
              compile
                ( LocalVariables
                    variables
                    Sequence.empty
                )
                Nothing
            jumpInstructionIndex <-
              gets (^. function . code)
                <&> Sequence.length
            bodySize <- compile body Nothing

            function . code
              %= Sequence.insertAt
                jumpInstructionIndex
                (Instruction.Jump bodySize)

            function . code
              %= ( |>
                     Instruction.GenericForLoop
                       (fromJust $ head' expressions)
                       (Sequence.length variables)
                 )

            mapM_
              free
              [expressions, variables]

            return bodySize
        )
        Nothing

    function . code
      %= ( |>
             Instruction.Jump
               (- bodySize - 2)
         )
  compile (CallStatement call) _ =
    compile
      @Call
      @Argument.Register
      call
      Nothing
      >>= free
        . Sequence.singleton

instance Compile Condition Int where
  compile (Condition expression body) _ = do
    let (condition, flag) = case expression of
          Unary Operation.Unary.Not condition -> (condition, True)
          _ -> (expression, False)

    condition' <-
      case condition of
        CallExpression call -> compile call (Just $ SavedReturnValues 1)
        _ -> compile condition Nothing

    function . code
      %= ( |>
             Instruction.Test
               condition'
               flag
         )

    free (Sequence.singleton condition')

    conditionIndex <-
      gets (^. function . code)
        <&> Sequence.length
    bodySize <- compile body Nothing
    bodyIndex <-
      gets (^. function . code)
        <&> Sequence.length

    function . code
      %= Sequence.insertAt
        conditionIndex
        (Instruction.Jump $ bodySize + 1)

    return bodyIndex

instance Compile ControlTransferStatement () where
  compile Break _ =
    gets _currentLoopBlockSize
      >>= \case
        Just size -> do
          currentSize <-
            gets (^. function . code)
              <&> Sequence.length

          function . code
            %= ( |>
                   Instruction.Jump (size - currentSize)
               )
        _ -> throw Error.BreakStatementOutsideLoop
  compile (Return ((CallExpression call) Sequence.:<| Sequence.Empty)) _ =
    void $
      compile
        @Call
        @Argument.Register
        call
        (Just TailCall)
  compile (Return values) _ = do
    registers <-
      mapM
        (`compile` Nothing)
        values

    function . code
      %= ( |>
             Instruction.Return
               (fromMaybe (Argument.Register 0) $ head' registers)
               (Sequence.length registers + 1)
         )

instance Compile Declaration (Sequence.Seq Argument.Register) where
  compile (LocalVariables identifiers expressions) _ =
    let (pairs, leftovers) = padRightAndZip Nothing identifiers (Just <$> expressions)
     in mapMWithIndex
          ( \index (name, expression) -> do
              register <-
                case expression of
                  Just (CallExpression call) ->
                    compile
                      call
                      ( Just $
                          SavedReturnValues $
                            if index + 1 == Sequence.length pairs - leftovers
                              then leftovers + 1
                              else 1
                      )
                  Just expression -> compile expression Nothing
                  _ -> allocate

              function . symbolMap %= Map.insert name register

              return register
          )
          pairs
  compile (Function name functionSkeleton) _ = do
    compile
      @Statement
      @()
      ( Assignment
          (Sequence.singleton name)
          (Sequence.singleton $ Closure functionSkeleton)
      )
      Nothing

    return Sequence.empty
  compile (LocalFunction name functionSkeleton) _ =
    compile
      ( LocalVariables
          (Sequence.singleton name)
          (Sequence.singleton $ Closure functionSkeleton)
      )
      Nothing
  compile (Method name functionSkeleton) _ = do
    compile
      @Statement
      @()
      ( Assignment
          (Sequence.singleton name)
          ( Sequence.singleton $
              Closure $
                functionSkeleton
                  & parameters
                  %~ (Text.pack "self" Sequence.<|)
          )
      )
      Nothing

    return Sequence.empty
