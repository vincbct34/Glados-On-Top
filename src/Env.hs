{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Env
-}

module Env where

import Types

data Env = Env {
    bindings :: [(String, LispValue)],
    parent :: Maybe Env
    } deriving (Show, Eq)

data EnvError
    = VariableNotFound String
    | VariableAlreadyDefined String
    deriving (Show, Eq)

-- Create an empty environment with no parent
emptyEnv :: Env
emptyEnv = Env [] Nothing

-- Lookup a variable in the environment chain
lookupVar :: String -> Env -> Maybe LispValue
lookupVar name (Env bindings parent) =
    case lookup name bindings of
        Just value -> Just value
        Nothing -> case parent of
            Just parentEnv -> lookupVar name parentEnv
            Nothing -> Nothing

-- Bind a new variable binding to the current environment
bindVar :: String -> LispValue -> Env -> Env
bindVar name value (Env bindings parent) =
    Env ((name, value) : bindings) parent

-- Extend environment with multiple bindings
extendEnv :: [(String, LispValue)] -> Env -> Env
extendEnv newBindings env = foldr (uncurry bindVar) env newBindings

-- //TODO Basic : newScope, newScopeWith