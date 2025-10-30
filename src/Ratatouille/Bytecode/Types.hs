{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Bytecode type definitions for Ratatouille VM
-}

module Ratatouille.Bytecode.Types
  ( Instruction (..),
    Bytecode,
    Value (..),
  )
where

import Data.Text (Text)

-- | Values that can be stored and manipulated by the VM
data Value
  = VInt Integer
  | VFloat Double     -- Float/double value
  | VString Text
  | VAtom Text
  | VTuple [Value]
  | VArray [Value]    -- Array value
  | VPid Integer -- Process ID
  | VUnit         -- Unit value for void operations
  | VNone         -- None value (null/absence AND Maybe constructor)
  | VBool Bool    -- Boolean value
  | VJust Value   -- Maybe value: Just x
  | VLeft Value   -- Either value: Left x (ko)
  | VRight Value  -- Either value: Right x (ok)
  deriving (Show, Eq)

-- | Bytecode instructions for the Nexus VM
data Instruction
  = -- Stack operations
    PUSH_INT Integer
  | PUSH_FLOAT Double  -- Push floating-point value
  | PUSH_STRING Text
  | PUSH_ATOM Text
  | PUSH_TUPLE Int -- Number of elements to pop and create tuple
  | PUSH_ARRAY Int -- Number of elements to pop and create array
  | PUSH_UNIT     -- Push unit value
  -- Variable operations (global scope)
  | LOAD_VAR Text
  | STORE_VAR Text
  -- Local variable operations (process-scoped)
  | LOAD_LOCAL Text
  | STORE_LOCAL Text
  -- Array operations
  | INDEX          -- Pop index and array, push array[index]
  | ARRAY_LENGTH   -- Pop array, push its length
  -- Process state operations
  | INIT_STATE     -- Initialize process state from stack top
  | GET_STATE      -- Push current process state to stack
  | SET_STATE      -- Set process state from stack top
  -- Arithmetic operations (work on both int and float)
  | ADD
  | SUB
  | MUL
  | DIV
  | CONCAT       -- String concatenation
  -- Increment/Decrement operations
  | INC_VAR Text     -- Increment variable, push new value (++x)
  | DEC_VAR Text     -- Decrement variable, push new value (--x)
  | INC_VAR_POST Text  -- Increment variable, push old value (x++)
  | DEC_VAR_POST Text  -- Decrement variable, push old value (x--)
  -- Comparison operations
  | CMP_EQ       -- Equality
  | CMP_NEQ      -- Not equal
  | CMP_LT       -- Less than
  | CMP_GT       -- Greater than
  | CMP_LTE      -- Less than or equal
  | CMP_GTE      -- Greater than or equal
  -- Logical operations
  | LOGIC_AND    -- Logical and
  | LOGIC_OR     -- Logical or
  -- Value operations
  | PUSH_NONE    -- Push none value (null/absence AND Maybe constructor)
  | PUSH_BOOL Bool  -- Push boolean value
  | GET_FIELD Text  -- Get field from tuple/record
  -- Maybe/Either operations
  | PUSH_JUST      -- Wrap top of stack in Just
  | PUSH_LEFT      -- Wrap top of stack in Left (ko)
  | PUSH_RIGHT     -- Wrap top of stack in Right (ok)
  | MAYBE_BIND Text  -- Bind Maybe monad with function
  | EITHER_BIND Text -- Bind Either monad with function
  -- Actor model operations
  | DEFINE_PROCESS Text [Text] Bytecode -- Name, params, body bytecode
  | CREATE_INSTANCE Text Int           -- Create process instance, push PID (name, arg count)
  | SEND                               -- Send message (receiver, message on stack)
  | WAIT_MESSAGE                       -- Wait for next message
  -- Pattern matching operations
  | MATCH_ATOM Text Int                -- Match atom, jump offset if no match
  | MATCH_VAR Text                     -- Match and bind variable
  | MATCH_TUPLE Int Int                -- Match tuple of size N, jump offset if no match
  | MATCH_WILDCARD                     -- Match anything (always succeeds)
  -- Process control
  | PROCESS_LOOP                       -- Main process message loop
  | SELF                               -- Push current process PID
  | EXIT_PROCESS                       -- Terminate current process
  -- Type casting operations
  | STATIC_CAST Text                   -- Static cast to type (safe, checked)
  | REINTERPRET_CAST Text              -- Reinterpret cast (unsafe, no checks)
  | CONST_CAST                         -- Const cast (removes const qualification)
  -- Control flow
  | JUMP Int                           -- Unconditional jump
  | JUMP_IF_FALSE Int                  -- Conditional jump
  | LABEL Text                         -- Jump target label
  | CALL Text                          -- Call function/process
  | RETURN
  | PRINT       -- Print value from stack
  | HALT
  deriving (Show, Eq)

-- | A sequence of bytecode instructions
type Bytecode = [Instruction]
