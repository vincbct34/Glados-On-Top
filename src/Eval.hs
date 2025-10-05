{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Eval
-}

module Eval
  ( eval,
    evalProgram,
    showResult,
  )
where

import Env
import Types

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
-- Support for function definition syntax: (define (name params...) body)
-- Transform it to: (define name (lambda params body))
eval (List [Atom "define", List (Atom name : params), body]) env =
  eval (List [Atom "define", Atom name, List [Atom "lambda", List params, body]]) env
eval (List [Atom "lambda", List params, body]) env =
  evalLambda params body env
eval (List [Atom "quote", expr]) env =
  Right (expr, env) -- Quote returns expression unevaluated
  -- Function application
eval (List (funcExpr : argExprs)) env =
  eval funcExpr env >>= \(funcValue, env') ->
    evalApplication funcValue argExprs env'

-------------------------------------------------------------------------------

-- | Special forms implementation

-- If conditional: (if condition then-expr else-expr)
evalIf :: LispValue -> LispValue -> LispValue -> Env -> Either String (LispValue, Env)
evalIf condition thenExpr elseExpr env =
  case eval condition env of
    Right (condValue, env') ->
      if isTruthy condValue
        then eval thenExpr env'
        else eval elseExpr env'
    Left err -> Left err

-- Define variable (define name value)
evalDefine :: String -> LispValue -> Env -> Either String (LispValue, Env)
evalDefine name expr env =
  case expr of
    -- Special handling for lambda definitions to enable recursion
    List [Atom "lambda", List paramExprs, body] ->
      mapM extractParamName paramExprs >>= \params ->
        if containsReference name body
          then
            let recursiveFunc = RecursiveFunction name params body env
                newEnv = bindVar name (Function recursiveFunc) env
             in Right (Function recursiveFunc, newEnv)
          else
            let userFunc = UserFunction params body env
                newEnv = bindVar name (Function userFunc) env
             in Right (Function userFunc, newEnv)
      where
        extractParamName (Atom paramName) = Right paramName
        extractParamName _ = Left "Lambda parameters must be atoms"
    -- Regular definitions
    _ ->
      eval expr env >>= \(value, env') ->
        Right (value, bindVar name value env')

-- Lambda function : (lambda (param1 param2) body)
evalLambda :: [LispValue] -> LispValue -> Env -> Either String (LispValue, Env)
evalLambda paramExprs body env =
  (\params -> (Function (UserFunction params body env), env))
    <$> mapM extractParamName paramExprs
  where
    extractParamName (Atom name) = Right name
    extractParamName _ = Left "Lambda parameters must be atoms"

-------------------------------------------------------------------------------

-- | Function application
evalApplication :: LispValue -> [LispValue] -> Env -> Either String (LispValue, Env)
evalApplication (Function (BuiltinFunction _ f)) argExprs env =
  evalArgs argExprs env >>= \(args, env') ->
    f args >>= \result ->
      Right (result, env')
evalApplication (Function (UserFunction params body capturedEnv)) argExprs env =
  evalArgs argExprs env >>= \(args, env') ->
    if length params /= length args
      then Left $ arityError params args
      else
        eval body (newScopeWith (zip params args) capturedEnv) >>= \(result, _) ->
          Right (result, env')
  where
    arityError expectedParams actualArgs =
      "Function expects "
        ++ show (length expectedParams)
        ++ " arguments, got "
        ++ show (length actualArgs)
evalApplication (Function (RecursiveFunction funcName params body capturedEnv)) argExprs env =
  evalArgs argExprs env >>= \(args, env') ->
    if length params /= length args
      then Left $ arityError params args
      else
        let selfRef = Function (RecursiveFunction funcName params body capturedEnv)
            funcEnv = newScopeWith ((funcName, selfRef) : zip params args) capturedEnv
         in eval body funcEnv >>= \(result, _) ->
              Right (result, env')
  where
    arityError expectedParams actualArgs =
      "Function expects "
        ++ show (length expectedParams)
        ++ " arguments, got "
        ++ show (length actualArgs)
evalApplication (Function (SpecialForm name _)) _ _ =
  Left $ "Special form " ++ name ++ " used incorrectly"
evalApplication _ _ _ =
  Left "Not a function"

-------------------------------------------------------------------------------

-- | Helper functions
containsReference :: String -> LispValue -> Bool
containsReference name (Atom atomName) = name == atomName
containsReference name (List exprs) = any (containsReference name) exprs
containsReference _ _ = False

-- Evaluate a list of expressions
evalArgs :: [LispValue] -> Env -> Either String ([LispValue], Env)
evalArgs [] env = Right ([], env)
evalArgs (x : xs) env =
  eval x env >>= \(val, env') ->
    evalArgs xs env' >>= \(vals, env'') ->
      Right (val : vals, env'')

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
evalProgram (x : xs) env =
  eval x env >>= \(_, env') ->
    evalProgram xs env'

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
showResult (Function (RecursiveFunction name params _ _)) = "<recursive-function:" ++ name ++ ":(" ++ unwords params ++ ")>"
showResult (Function (SpecialForm name _)) = "<special:" ++ name ++ ">"
