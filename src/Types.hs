{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Types
-}

module Types (
    -- * AST Types
    LispValue(..),
    LispFunction(..),
    -- * Environment Types
    Env(..),
    -- * Helper functions
    isAtom,
    isList,
    isNumber,
    isString,
    isBoolean,
    isFunction
) where

-------------------------------------------------------------------------------
-- | Represents a LISP value in our AST
data LispValue
    = Atom String              -- ^ Symbols/identifiers (e.g., +, factorial, x)
    | Number Integer           -- ^ Integer numbers (e.g., 42, -17)
    | String String            -- ^ String literals (e.g., "hello")
    | Boolean Bool             -- ^ Boolean values (#t, #f)
    | List [LispValue]         -- ^ Lists and function calls (e.g., (+ 1 2))
    | Function LispFunction    -- ^ Function values
    | Nil                      -- ^ Empty list / null value
    deriving (Show, Eq)

-- | Environment for variable bindings with lexical scoping
data Env = Env {
    bindings :: [(String, LispValue)],
    parent :: Maybe Env
    } deriving (Show, Eq)

-------------------------------------------------------------------------------

-- | Represents different types of functions in LISP
data LispFunction
    = BuiltinFunction String ([LispValue] -> Either String LispValue)  -- ^ Built-in functions (+, -, etc.)
    | UserFunction [String] LispValue Env                              -- ^ User-defined functions (lambda) with captured environment
    | RecursiveFunction String [String] LispValue Env                  -- ^ Recursive functions with self-reference
    | SpecialForm String ([LispValue] -> Either String LispValue)      -- ^ Special forms (if, define, lambda)

instance Show LispFunction where
    show (BuiltinFunction name _) = "<builtin:" ++ name ++ ">"
    show (UserFunction params _ _) = "<function:(" ++ unwords params ++ ")>"
    show (RecursiveFunction name params _ _) = "<recursive-function:" ++ name ++ ":(" ++ unwords params ++ ")>"
    show (SpecialForm name _) = "<special:" ++ name ++ ">"

instance Eq LispFunction where
    (BuiltinFunction n1 _) == (BuiltinFunction n2 _) = n1 == n2
    (UserFunction p1 b1 e1) == (UserFunction p2 b2 e2) = p1 == p2 && b1 == b2 && e1 == e2
    (RecursiveFunction n1 p1 b1 e1) == (RecursiveFunction n2 p2 b2 e2) = n1 == n2 && p1 == p2 && b1 == b2 && e1 == e2
    (SpecialForm n1 _) == (SpecialForm n2 _) = n1 == n2
    _ == _ = False

-------------------------------------------------------------------------------
-- | Helper functions for type checking

-- | Check if a LispValue is an atom
isAtom :: LispValue -> Bool
isAtom (Atom _) = True
isAtom _ = False

-- | Check if a LispValue is a list
isList :: LispValue -> Bool
isList (List _) = True
isList Nil = True
isList _ = False

-- | Check if a LispValue is a number
isNumber :: LispValue -> Bool
isNumber (Number _) = True
isNumber _ = False

-- | Check if a LispValue is a string
isString :: LispValue -> Bool
isString (String _) = True
isString _ = False

-- | Check if a LispValue is a boolean
isBoolean :: LispValue -> Bool
isBoolean (Boolean _) = True
isBoolean _ = False

-- | Check if a LispValue is a function
isFunction :: LispValue -> Bool
isFunction (Function _) = True
isFunction _ = False
