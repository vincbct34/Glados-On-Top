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

main :: IO ()
main = do
    input <- getContents
    case parseMultipleExpressions input of
        Left parseErr -> do
            putStrLn $ "Parse Error: " ++ show parseErr
        Right expressions -> do
            _ <- evaluateExpressions expressions builtinEnv
            return ()

-- Parse multiple expressions from input
parseMultipleExpressions :: String -> Either ParserError [LispValue]
parseMultipleExpressions = parseExpressions

-- Parse a sequence of LISP expressions with proper whitespace and comment handling
parseExpressions :: String -> Either ParserError [LispValue]
parseExpressions input = 
    case runParser parseExpressionsParser input of
        Left err -> Left err
        Right (exprs, _) -> Right exprs
  where
    parseExpressionsParser = do
        parseWhitespace  -- Skip initial whitespace and comments
        exprs <- parseMany (parseWhitespace *> parseLispValue <* parseWhitespace)
        parseWhitespace  -- Skip final whitespace and comments
        parseEOF
        return exprs

-- Evaluate multiple expressions in sequence, showing each result
evaluateExpressions :: [LispValue] -> Env -> IO (Maybe LispValue, Env)
evaluateExpressions [] env = return (Nothing, env)
evaluateExpressions [expr] env = do
    putStrLn $ "=> " ++ showLispValue expr
    case eval expr env of
        Left evalErr -> do
            putStrLn $ "Eval Error: " ++ evalErr
            return (Nothing, env)
        Right (result, newEnv) -> do
            putStrLn $ showResult result
            return (Just result, newEnv)
evaluateExpressions (expr:exprs) env = do
    putStrLn $ "=> " ++ showLispValue expr
    case eval expr env of
        Left evalErr -> do
            putStrLn $ "Eval Error: " ++ evalErr
            return (Nothing, env)
        Right (result, newEnv) -> do
            putStrLn $ showResult result
            evaluateExpressions exprs newEnv

-- Helper function to display LISP expressions nicely
showLispValue :: LispValue -> String
showLispValue (Number n) = show n
showLispValue (Boolean True) = "#t"
showLispValue (Boolean False) = "#f"
showLispValue (String s) = "\"" ++ s ++ "\""
showLispValue (Atom a) = a
showLispValue Nil = "()"
showLispValue (List xs) = "(" ++ unwords (map showLispValue xs) ++ ")"
showLispValue (Function f) = show f