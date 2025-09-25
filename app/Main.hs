{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Main
-}

module Main (main) where

import Parser
import Types
import Eval
import Builtins
import Env
import System.IO (hFlush, stdout)
import Control.Monad (when)

main :: IO ()
main = do
    input <- getContents
    case parseMultipleExpressions input of
        Left parseErr -> do
            putStrLn $ "Parse Error: " ++ show parseErr
        Right expressions -> do
            (lastResult, _) <- evaluateExpressions expressions builtinEnv
            case lastResult of
                Just result -> putStrLn $ showResult result
                Nothing -> return ()

-- Parse multiple expressions from input
parseMultipleExpressions :: String -> Either ParserError [LispValue]
parseMultipleExpressions input = parseExpressions (trim input)
  where
    trim = dropWhile (== ' ') . dropWhile (== '\n') . dropWhile (== '\t')

-- Parse a sequence of LISP expressions
parseExpressions :: String -> Either ParserError [LispValue]
parseExpressions "" = Right []
parseExpressions input = 
    case runParser parseLispValue (skipWhitespace input) of
        Left err -> Left err
        Right (expr, remaining) -> 
            case parseExpressions (skipWhitespace remaining) of
                Left err -> Left err
                Right exprs -> Right (expr : exprs)

-- Skip whitespace in input
skipWhitespace :: String -> String
skipWhitespace = dropWhile (`elem` " \t\n\r")

-- Evaluate multiple expressions in sequence
evaluateExpressions :: [LispValue] -> Env -> IO (Maybe LispValue, Env)
evaluateExpressions [] env = return (Nothing, env)
evaluateExpressions [expr] env = do
    case eval expr env of
        Left evalErr -> do
            putStrLn $ "Eval Error: " ++ evalErr
            return (Nothing, env)
        Right (result, newEnv) -> return (Just result, newEnv)
evaluateExpressions (expr:exprs) env = do
    case eval expr env of
        Left evalErr -> do
            putStrLn $ "Eval Error: " ++ evalErr
            return (Nothing, env)
        Right (_, newEnv) -> evaluateExpressions exprs newEnv