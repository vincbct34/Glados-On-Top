{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Main
-}

module Main (main) where

import Parser
import Types

main :: IO ()
main = do
    putStrLn "Lisp Parser REPL - Type 'quit' to exit."
    repl

repl :: IO ()
repl = do
    putStr "Lisp> "
    line <- getLine
    if line == "quit"
        then putStrLn "Goodbye!"
        else do
            case runParser parseExpression line of
                Left err -> putStrLn $ "Error: " ++ show err
                Right (ast, _) -> putStrLn $ "Parsed AST: " ++ show ast
            repl