{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Builtins
-}

module Builtins (
    builtinEnv,
    isBuiltin
) where

import Types
import Env

-------------------------------------------------------------------------------
-- | Create an environment pre-populated with all builtin functions
builtinEnv :: Env
builtinEnv = foldl addBuiltinGroup emptyEnv [
    arithmeticBuiltins,
    comparisonBuiltins,
    listBuiltins,
    predicateBuiltins
  ]
  where
    addBuiltinGroup env builtins = extendEnv builtins env

-- Arithmetic operations
arithmeticBuiltins :: [(String, LispValue)]
arithmeticBuiltins = [
    ("+", Function (BuiltinFunction "+" addBuiltin)),
    ("-", Function (BuiltinFunction "-" subBuiltin)),
    ("*", Function (BuiltinFunction "*" mulBuiltin)),
    ("/", Function (BuiltinFunction "/" divBuiltin))
]

-- Comparison operations
comparisonBuiltins :: [(String, LispValue)]
comparisonBuiltins = [
    ("=", Function (BuiltinFunction "=" eqBuiltin)),
    ("<", Function (BuiltinFunction "<" ltBuiltin)),
    (">", Function (BuiltinFunction ">" gtBuiltin)),
    ("<=", Function (BuiltinFunction "<=" leBuiltin)),
    (">=", Function (BuiltinFunction ">=" geBuiltin))
]

-- List operations
listBuiltins :: [(String, LispValue)]
listBuiltin= [
    ("car", Function (BuiltinFunction "car" carBuiltin)),
    ("cdr", Function (BuiltinFunction "cdr" cdrBuiltin)),
    ("cons", Function (BuiltinFunction "cons" consBuiltin))
]

-- Type predicates
predicateBuiltins :: [(String, LispValue)]
predicateBuiltins = [
    ("null?", Function (BuiltinFunction "null?" nullBuiltin)),
    ("number?", Function (BuiltinFunction "number?" numBuiltin)),
    ("list?", Function (BuiltinFunction "list?" listBuiltin)),
    ("atom?", Function (BuiltinFunction "atom?" atomBuiltin))
]

-- Check if the string is a builtin function
isBuiltin :: String :: Bool
isBuiltin name = isDefined name builtinEnv

-- Helper to extract numbers
extractNumber :: LispValue -> Either String Integer
extractNumber (Number n) = Right n
extractNumber x = Left $ "Expected number, got: " ++ show x

-------------------------------------------------------------------------------
-- | ARITHMETIC FUNCTIONS

-- Addition
addBuiltin :: [LispValue] -> Either String LispValue
addBuiltin [] = Right (Number 0)
addBuiltin args = do
    nums <- mapM extractNumber args
    return $ Number (sum nums)

-- Subtraction
subBuiltin :: [LispValue] -> Either String LispValue
subBuiltin [] = Left "- requires at least one argument"
subBuiltin [x] = do
    n <- extractNumber x
    return $ Number (-n)
subBuiltin (x:xs) = do
    first <- extractNumber x
    rest <- mapM extractNumber xs
    return $ Number (first - sum rest)

-- Multiplication
mulBuiltin :: [LispValue] -> Either String LispValue
mulBuiltin [] = Right (Number 1)
mulBuiltin args = do
    nums <- mapM extractNumber args
    return $ Number (product nums)

-- Division
divBuiltin :: [LispValue] -> Either String LispValue
divBuiltin [] = Left "/ requires at least one argument"
divBuiltin [x] = do
    n <- extractNumber x
    if n == 0 then Left "Division by zero"
    else return $ Number (1 `div` n)
divBuiltin (x:xs) = do
    first <- extractNumber x
    rest <- mapM extractNumber xs
    if any (==0) rest
        then Left "Division by zero"
        else return $ Number (foldl div first rest)

-------------------------------------------------------------------------------
-- | COMPARISON FUNCTIONS

-- Equality
eqBuiltin :: [LispValue] -> Either String LispValue
eqBuiltin [] = Right (Boolean True)
eqBuiltin [_] = Right (Boolean True)
eqBuiltin (x:xs) = Right $ Boolean (all (== x) xs)

-- Less than
ltBuiltin :: [LispValue] -> Either String LispValue
ltBuiltin [] = Right (Boolean True)
ltBuiltin [_] = Right (Boolean True)
ltBuiltin args = do
    nums <- mapM extractNumber args
    return $ Boolean (isStrictlyIncreasing nums)

-- Greater than
gtBuiltin :: [LispValue] -> Either String LispValue
gtBuiltin [] = Right (Boolean True)
gtBuiltin [_] = Right (Boolean True)
gtBuiltin args = do
    nums <- mapM extractNumber args
    return $ Boolean (isStrictlyDecreasing nums)

-- Less than or equal
leBuiltin :: [LispValue] -> Either String LispValue
leBuiltin [] = Right (Boolean True)
leBuiltin [_] = Right (Boolean True)
leBuiltin args = do
    nums <- mapM extractNumber args
    return $ Boolean (isNonDecreasing nums)

-- Greater than or equal
geBuiltin :: [LispValue] -> Either String LispValue
geBuiltin [] = Right (Boolean True)
geBuiltin [_] = Right (Boolean True)
geBuiltin args = do
    nums <- mapM extractNumber args
    return $ Boolean (isNonIncreasing nums)

-------------------------------------------------------------------------------
-- | HELPER FUNCTIONS FOR COMPARISONS

-- Check if the list is strictly increasing
isStrictlyIncreasing :: [Integer] -> Bool
isStrictlyIncreasing [] = True
isStrictlyIncreasing [_] = True
isStrictlyIncreasing (x:y:xs) = x < y && isStrictlyIncreasing (y:xs)

-- Check if the list is strictly decreasing
isStrictlyDecreasing :: [Integer] -> Bool
isStrictlyDecreasing [] = True
isStrictlyDecreasing [_] = True
isStrictlyDecreasing (x:y:xs) = x > y && isStrictlyDecreasing (y:xs)

-- Check if the list is non-decreasing
isNonDecreasing :: [Integer] -> Bool
isNonDecreasing [] = True
isNonDecreasing [_] = True
isNonDecreasing (x:y:xs) = x <= y && isNonDecreasing (y:xs)

-- Check if the list is non-increasing
isNonIncreasing :: [Integer] -> Bool
isNonIncreasing [] = True
isNonIncreasing [_] = True
isNonIncreasing (x:y:xs) = x >= y && isNonIncreasing (y:xs)

-------------------------------------------------------------------------------
-- | LIST OPERATIONS

-- Get first element
carBuiltin :: [LispValue] -> Either String LispValue
carBuiltin [List []] = Left "car: cannot take car of empty list"
carBuiltin [List (x:_)] = Right x
carBuiltin [Nil] = Left "car: cannot take car of empty list"
carBuiltin [_] = Left "car: argument must be a list"
carBuiltin _ = Left "car: expects exactly one argument"

-- Get rest of the list
cdrBuiltin :: [LispValue] -> Either String LispValue
cdrBuiltin [List []] = Left "cdr: cannot take cdr of empty list"
cdrBuiltin [List [_]] = Right Nil
cdrBuiltin [List (_:xs)] = Right $ List xs
cdrBuiltin [Nil] = Left "cdr: cannot take cdr of nil"
cdrBuiltin [_] = Left "cdr: argument must be a list"
cdrBuiltin _ = Left "cdr: expects exactly one argument"

-- Construct a new list
consBuiltin :: [LispValue] -> Either String LispValue
consBuiltin [x, List xs] = Right $ List (x:xs)
consBuiltin [x, Nil] = Right $ List [x]
consBuiltin [_, _] = Left "cons: second argument must be a list"
consBuiltin _ = Left "cons: expects exactly two arguments"

-------------------------------------------------------------------------------
-- | TYPE PREDICATES

-- Check if null/empty
nullBuiltin :: [LispValue] -> Either String LispValue
nullBuiltin [Nil] = Right (Boolean True)
nullBuiltin [List []] = Right (Boolean True)
nullBuiltin [_] = Right (Boolean False)
nullBuiltin _ = Left "null?: expects exactly one argument"

-- Check if number
numBuiltin :: [LispValue] -> Either String LispValue
numBuiltin [Number _ ] = Right (Boolean True)
numBuiltin [_] = Right (Boolean False)
numBuiltin _ = Left "number?: expects exactly one argument"


-- Check if list
listBuiltin :: [LispValue] -> Either String LispValue
listBuiltin [List _] = Right (Boolean True)
listBuiltin [Nil] = Right (Boolean True)
listBuiltin [_] = Right (Boolean False)
listBuiltin _ = Left "list?: expects exactly one argument"

-- Check if atom
atomBuiltin :: [LispValue] -> Either String LispValue
atomBuiltin [Atom _] = Right (Boolean True)
atomBuiltin [_] = Right (Boolean False)
atomBuiltin _ = Left "atom?: expects exactly one argument"