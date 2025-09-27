{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Env
-}

module Env (
    Env (..),
    EnvError (..),
    emptyEnv,
    lookupVar,
    bindVar,
    extendEnv,
    newScope,
    newScopeWith,
    isDefined
) where

import Types

data EnvError
    = VariableNotFound String
    | VariableAlreadyDefined String
    deriving (Show, Eq)

-- Create an empty environment with no parent
emptyEnv :: Env
emptyEnv = Env [] Nothing

-- Lookup a variable in the environment chain
lookupVar :: String -> Env -> Maybe LispValue
lookupVar name (Env envBindings envParent) =
    case lookup name envBindings of
        Just value -> Just value
        Nothing -> case envParent of
            Just parentEnv -> lookupVar name parentEnv
            Nothing -> Nothing

-- Bind a new variable binding to the current environment
bindVar :: String -> LispValue -> Env -> Env
bindVar name value (Env envBindings envParent) =
    Env ((name, value) : envBindings) envParent

-- Extend environment with multiple bindings
extendEnv :: [(String, LispValue)] -> Env -> Env
extendEnv newBindings env = foldr (uncurry bindVar) env newBindings

-- Create a new child scope (for function calls, let expressions)
newScope :: Env -> Env
newScope parentEnv = Env [] (Just parentEnv)

-- Create a new child scope with initial bindings
newScopeWith :: [(String, LispValue)] -> Env -> Env
newScopeWith scopeBindings parentEnv = Env scopeBindings (Just parentEnv)

-- Check if variable exists
isDefined :: String -> Env -> Bool
isDefined name env = case lookupVar name env of
    Just _ -> True
    Nothing -> False

