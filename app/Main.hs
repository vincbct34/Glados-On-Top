{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Main entry point - Parse and compile .rat files
-}

module Main (main) where

import qualified Data.Text.IO as TIO
import Ratatouille.Bytecode.Compiler (compileProgram)
import Ratatouille.Bytecode.Encoder (writeBinaryFile)
import Ratatouille.Bytecode.Decoder (readBinaryFile)
import Ratatouille.Parser.Proc (pProgram)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (replaceExtension, takeExtension)
import Text.Megaparsec (errorBundlePretty, parse)
import Control.Monad (when)

import qualified Ratatouille.Error.Format as ErrFmt
import Ratatouille.Error.Types (makeRichError, RichError)

-- Use color helpers from ErrFmt
colorize :: String -> String -> String
colorize = ErrFmt.colorize

formatRichError :: RichError -> String
formatRichError = ErrFmt.formatRichError

putStrLnColored :: String -> String -> IO ()
putStrLnColored c txt = putStrLn (colorize c txt)

data CompileMode = ShowBytecode | WriteBinary FilePath | InspectBinary

data CompilerOptions = CompilerOptions
  { optInputFile :: FilePath
  , optMode :: CompileMode
  , optVerbose :: Bool
  }

processFile :: CompilerOptions -> IO ()
processFile opts = do
  let filePath = optInputFile opts
      mode = optMode opts
      verbose = optVerbose opts
  
  -- Check if we're inspecting a binary file
  if takeExtension filePath == ".rtbc"
    then inspectBinaryFile filePath
    else compileSourceFile filePath mode verbose

inspectBinaryFile :: FilePath -> IO ()
inspectBinaryFile filePath = do
  putStrLnColored ("\ESC[1m\ESC[36m") ("Inspecting binary file: " ++ filePath)
  putStrLn ""
  
  result <- readBinaryFile filePath
  case result of
    Left err -> do
      putStrLnColored ("\ESC[1m\ESC[31m") ("Error reading binary file: " ++ err)
      exitFailure
    Right bytecode -> do
      putStrLnColored ("\ESC[1m\ESC[32m") "Binary file decoded successfully"
      putStrLn ""
      putStrLnColored ("\ESC[1m\ESC[33m") "BYTECODE:"
      mapM_ (putStrLn . ("  " ++) . show) bytecode
      putStrLn ""
      putStrLnColored ("\ESC[1m\ESC[36m") "SUMMARY:"
      putStrLn $ "  Total instructions: " ++ show (length bytecode)
      exitSuccess

compileSourceFile :: FilePath -> CompileMode -> Bool -> IO ()
compileSourceFile filePath mode verbose = do
      
  putStrLnColored ("\ESC[1m\ESC[36m") ("Reading file: " ++ filePath)
  content <- TIO.readFile filePath

  putStrLn ""
  putStrLnColored ("\ESC[1m\ESC[34m") "PARSING PHASE"
  putStrLn ""
  case parse pProgram filePath content of
    Left err -> do
      putStrLnColored ("\ESC[1m\ESC[31m") "PARSE ERROR DETECTED"
      putStrLn ""
      let errMsgStr = errorBundlePretty err
          rich = makeRichError filePath errMsgStr content
      putStrLn $ formatRichError rich
      putStrLn ""
      exitFailure
    Right ast -> do
      putStrLnColored ("\ESC[1m\ESC[32m") "Parsing successful"
      putStrLn ""
      
      when verbose $ do
        putStrLnColored ("\ESC[1m\ESC[33m") "AST:"
        print ast
        putStrLn ""

      putStrLnColored ("\ESC[1m\ESC[34m") "COMPILATION PHASE"
      let bytecode = compileProgram ast
      putStrLnColored ("\ESC[1m\ESC[32m") "Compilation successful"
      
      case mode of
        ShowBytecode -> do
          putStrLn ""
          putStrLnColored ("\ESC[1m\ESC[33m") "BYTECODE:"
          mapM_ print bytecode
          putStrLn ""
          exitSuccess
        
        WriteBinary outputPath -> do
          writeBinaryFile outputPath bytecode
          putStrLn ""
          putStrLnColored ("\ESC[1m\ESC[32m") ("✓ Binary file written: " ++ outputPath)
          putStrLn ""
          putStrLnColored ("\ESC[1m\ESC[36m") "SUMMARY:"
          putStrLn $ "  Input file:    " ++ filePath
          putStrLn $ "  Output file:   " ++ outputPath
          putStrLn $ "  Instructions:  " ++ show (length bytecode)
          putStrLn $ "  File size:     (binary format)"
          putStrLn ""
          exitSuccess
        
        InspectBinary -> 
          -- This case should never happen here since .rtbc files go through inspectBinaryFile
          putStrLnColored ("\ESC[1m\ESC[31m") "Error: InspectBinary mode in wrong context" >> exitFailure

parseArgs :: [String] -> Either String CompilerOptions
parseArgs args = 
  let (flags, files) = partition (\arg -> arg == "--verbose" || arg == "-v" || arg == "--show-bytecode" || arg == "--show-ast" || arg == "--inspect") args
      verbose = "--verbose" `elem` flags || "-v" `elem` flags || "--show-ast" `elem` flags
      showBytecodeFl = "--show-bytecode" `elem` flags
      inspectFl = "--inspect" `elem` flags
      
      nonFlags = filter (\arg -> not (arg `elem` ["-o", "--output"])) files
      
  in case (nonFlags, showBytecodeFl, inspectFl) of
    ([], _, _) -> Left "No input file provided"
    ([inputFile], _, True) -> Right $ CompilerOptions inputFile InspectBinary verbose
    ([inputFile], True, False) -> Right $ CompilerOptions inputFile ShowBytecode verbose
    ([inputFile], False, False) -> 
      -- Auto-detect: if .rtbc file, inspect it; otherwise compile it
      if takeExtension inputFile == ".rtbc"
        then Right $ CompilerOptions inputFile InspectBinary verbose
        else 
          let outputFile = replaceExtension inputFile ".rtbc"
          in Right $ CompilerOptions inputFile (WriteBinary outputFile) verbose
    _ -> 
      -- Handle -o / --output flags
      case args of
        _ | "-o" `elem` args || "--output" `elem` args ->
          case parseOutputArgs args of
            Just (input, output) -> Right $ CompilerOptions input (WriteBinary output) verbose
            Nothing -> Left "Invalid -o/--output usage"
        _ -> Left "Invalid arguments"

-- Helper to parse -o/--output arguments
parseOutputArgs :: [String] -> Maybe (FilePath, FilePath)
parseOutputArgs args = findOutput args
  where
    findOutput [] = Nothing
    findOutput ("-o":out:_) = Just (findInput (filter (`notElem` ["-o", out]) args), out)
    findOutput ("--output":out:_) = Just (findInput (filter (`notElem` ["--output", out]) args), out)
    findOutput (_:rest) = findOutput rest
    
    findInput xs = case filter (\x -> not (x `elem` ["--verbose", "-v", "--show-bytecode", "--show-ast", "--inspect"])) xs of
      (i:_) -> i
      [] -> ""

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p xs = (filter p xs, filter (not . p) xs)

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left errMsg -> do
      putStrLnColored ("\ESC[1m\ESC[31m") errMsg
      putStrLn ""
      putStrLn "Usage:"
      putStrLn "  glados <input.rat>                      # Compile to .rtbc file"
      putStrLn "  glados <input.rat> -o <output.rtbc>     # Compile to specific file"
      putStrLn "  glados <input.rat> --show-bytecode      # Show bytecode without writing file"
      putStrLn "  glados <input.rat> --verbose            # Show AST and verbose output"
      putStrLn "  glados <input.rat> --show-ast           # Show AST (alias for --verbose)"
      putStrLn "  glados <file.rtbc>                      # Inspect compiled binary file"
      putStrLn "  glados <file.rtbc> --inspect            # Inspect binary file (explicit)"
      exitFailure
    Right opts -> processFile opts
