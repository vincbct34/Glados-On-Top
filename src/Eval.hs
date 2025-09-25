{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Eval
-}

module Eval (
    eval,
    evalProgram,
    showResult
) where

import Types
import Env
import Builtins

-------------------------------------------------------------------------------
-- | Error types for evaluations
data EvalError
    = UndefinedVariable String
    | NotAFunction String
    | ArgumentError String
    | RuntimeError String
    deriving (Show, Eq)

-- Main evaluation function
eval :: LispValue -> Env -> Either String (LispValue, Env)
-- Literals
eval (Number n) env = Right (Number n, env) 
eval (Boolean b) env = Right (Boolean b, env)
eval (String s) env = Right (String s, env)
eval Nil env = Right (Nil, env)
-- Variable lookup
eval (Atom name) env =
    case lookupVar name env of     
        Just value -> Right (value, env)
        Nothing -> Left ("Undefined variable: " ++ name)
-- Function already evaluated
eval (Function f) env = Right (Function f, env)
-- List evaluation (function application or special forms)
-- Empty list evaluates to Nil
eval (List []) env = Right (Nil, env)
-- Special forms
eval (List [Atom "if", condition, thenExpr, elseExpr]) env =
    evalIf condition thenExpr elseExpr env
eval (List [Atom "define", Atom name, expr]) env =
    evalDefine name expr env
eval (List [Atom "lambda", List params, body]) env =
    evalLambda params body env
eval (List [Atom "quote", expr]) env =
    Right (expr, env) -- Quote returns expression unevaluated
-- Function application
eval (List (funcExpr:argExprs)) env = do
    (funcValue, env') <- eval funcExpr env
    evalApplication funcValue argExprs env'



-------------------------------------------------------------------------------
-- | Special forms implementation

-- If conditional: (if condition then-expr else-expr)
evalIf :: LispValue -> LispValue -> LispValue -> Env -> Either String (LispValue, Env)
evalIf condition thenExpr elseExpr env = do
    (condValue, env') <- eval condition env
    if isTruthy condValue
        then eval thenExpr env'
        else eval elseExpr env'

-- Define variable (define name value)
evalDefine :: String -> LispValue -> Env -> Either String (LispValue, Env)
evalDefine name expr env = do
    (value, env') <- eval expr env
    let newEnv = bindVar name value env'
    Right (value, newEnv)

-- Lambda function : (lambda (param1 param2) body)
evalLambda :: [LispValue] -> LispValue -> Env -> Either String (LispValue, Env)
evalLambda paramExprs body env = do
    params <- mapM extractParamName paramExprs
    let userFunc = UserFunction params body env  -- Capture current environment
    Right (Function userFunc, env)
    where
        extractParamName (Atom name) = Right name
        extractParamName _ = Left "Lambda parameters must be atoms"

-------------------------------------------------------------------------------
-- | Function application

evalApplication :: LispValue -> [LispValue] -> Env -> Either String (LispValue, Env)
evalApplication (Function (BuiltinFunction name f)) argExprs env = do
    -- Evaluate all arguments for builtin functions
    (args, env') <- evalArgs argExprs env
    result <- f args
    Right (result, env')
evalApplication (Function (UserFunction params body capturedEnv)) argExprs env = do
    -- Evaluate arguments
    (args, env') <- evalArgs argExprs env
    -- Check arity
    if length params /= length args
        then Left $ "Function expects " ++ show (length params) ++
            " arguments, got " ++ show (length args)
        else do
            -- Create new scope with parameter bindings using captured environment
            let paramBindings = zip params args
            let funcEnv = newScopeWith paramBindings capturedEnv
            -- Evaluate body in new environment (with captured closure)
            (result, _) <- eval body funcEnv
            -- Return result with original environment (lexical scoping)
            Right (result, env')
evalApplication (Function (SpecialForm name _)) _ _ =
    Left $ "Special form " ++ name ++ " used incorrectly"
evalApplication _ _ _ =
    Left "Not a function"

-------------------------------------------------------------------------------
-- | Helper functions

-- Evaluate a list of expressions
evalArgs :: [LispValue] -> Env -> Either String ([LispValue], Env)
evalArgs [] env = Right ([], env) 
evalArgs (x:xs) env = do
    (val, env') <- eval x env
    (vals, env'') <- evalArgs xs env'
    Right (val:vals, env'')

-- Check if a value is truthy
isTruthy :: LispValue -> Bool
isTruthy (Boolean False) = False
isTruthy Nil = False
isTruthy (List []) = False
isTruthy _ = True

-- Evaluate multiple expressions in sequence
evalProgram :: [LispValue] -> Env -> Either String (LispValue, Env)
evalProgram [] env = Right (Nil, env)
evalProgram [expr] env = eval expr env
evalProgram (x:xs) env = do
    (_, env') <- eval x env
    evalProgram xs env

-------------------------------------------------------------------------------
-- | Pretty printing for results

-- Show evaluation result prettier
showResult :: LispValue -> String
showResult (Number n) = show n
showResult (Boolean True) = "#t"
showResult (Boolean False) = "#f"
showResult (String s) = "\"" ++ s ++ "\""
showResult (Atom a) = a
showResult Nil = "()"
showResult (List xs) = "(" ++ unwords (map showResult xs) ++ ")"
showResult (Function (BuiltinFunction name _)) = "<builtin:" ++ name ++ ">"
showResult (Function (UserFunction params _ _)) = "<function:(" ++ unwords params ++ ")>"
showResult (Function (SpecialForm name _)) = "<special:" ++ name ++ ">"

-- Evaluate and show result
evalAndShow :: LispValue -> Env -> IO (Maybe Env)
evalAndShow expr env =
    case eval expr env of
        Left err -> do
            putStrLn $ "Error: " ++ err
            return (Just env)
        Right (result, newEnv) -> do
            putStrLn $ showResult result
            return (Just newEnv)
