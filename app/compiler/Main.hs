{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Main entry point - Parse and compile .rat files
-}

module Main (main) where

import qualified Data.Text.IO as TIO
import Ratatouille.Bytecode (compileProgram)
import Ratatouille.Parser.Proc (pProgram)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (replaceExtension)
import Text.Megaparsec (errorBundlePretty, parse)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "Usage: Glados-On-Top-exe <file.rat> [-o output.gbc]"
      putStrLn "       Parse and compile a Ratatouille source file to bytecode"
      putStrLn ""
      putStrLn "Options:"
      putStrLn "  -o <file>    Specify output bytecode file (default: <input>.gbc)"
      exitFailure
    _ -> do
      let (inputFile, outputFile) = parseArgs args
      processFile inputFile outputFile

parseArgs :: [String] -> (FilePath, FilePath)
parseArgs args =
  case args of
    [input] -> (input, replaceExtension input ".gbc")
    [input, "-o", output] -> (input, output)
    (input : "-o" : output : _) -> (input, output)
    (input : _) -> (input, replaceExtension input ".gbc")
    _ -> error "Invalid arguments"

processFile :: FilePath -> FilePath -> IO ()
processFile inputPath outputPath = do
  content <- TIO.readFile inputPath

  case parse pProgram inputPath content of
    Left err -> do
      putStrLn $ "Error: Failed to parse " ++ inputPath
      putStrLn $ errorBundlePretty err
      exitFailure
    Right ast -> do
      let bytecode = compileProgram ast

      writeFile outputPath $ unlines $ map show bytecode

      putStrLn $ "Compiled successfully: " ++ inputPath ++ " -> " ++ outputPath
      exitSuccess
