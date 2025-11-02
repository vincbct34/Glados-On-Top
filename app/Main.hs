{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Main entry point - Parse and compile .rat files
-}

module Main (main) where

import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Ratatouille.AST (Program)
import Ratatouille.Bytecode.Compiler (compileProgram)
import Ratatouille.Bytecode.Decoder (readBinaryFile)
import Ratatouille.Bytecode.Encoder (writeBinaryFile)
import Ratatouille.Bytecode.Types (Bytecode)
import qualified Ratatouille.Error.Format as ErrFmt
import Ratatouille.Error.Types (RichError, makeRichError)
import Ratatouille.Parser.Proc (pProgram)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (replaceExtension, takeExtension)
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty, parse)
import Data.Void (Void)

-- | Apply ANSI color codes to text
colorize :: String -> String -> String
colorize = ErrFmt.colorize

-- | Format rich error with context
formatRichError :: RichError -> String
formatRichError = ErrFmt.formatRichError

-- | Print colored text to stdout
putStrLnColored :: String -> String -> IO ()
putStrLnColored c txt = putStrLn (colorize c txt)

-- | Compilation mode selection
data CompileMode = ShowBytecode | WriteBinary FilePath | InspectBinary

-- | Compiler command-line options
data CompilerOptions = CompilerOptions
  { optInputFile :: FilePath
  , optMode :: CompileMode
  , optVerbose :: Bool
  }

-- | Process a file based on compiler options
processFile :: CompilerOptions -> IO ()
processFile opts
  | takeExtension filePath == ".rtbc" = inspectBinaryFile filePath
  | otherwise = compileSourceFile filePath mode verbose
  where
    filePath = optInputFile opts
    mode = optMode opts
    verbose = optVerbose opts

-- | Inspect and display contents of a binary bytecode file
inspectBinaryFile :: FilePath -> IO ()
inspectBinaryFile filePath = do
  printInspectionHeader filePath
  result <- readBinaryFile filePath
  case result of
    Left err -> handleReadError err
    Right bytecode -> displayBytecode bytecode

-- | Print inspection header
printInspectionHeader :: FilePath -> IO ()
printInspectionHeader filePath = do
  putStrLnColored "\ESC[1m\ESC[36m"
    ("Inspecting binary file: " ++ filePath)
  putStrLn ""

-- | Handle binary read error
handleReadError :: String -> IO ()
handleReadError err = do
  putStrLnColored "\ESC[1m\ESC[31m"
    ("Error reading binary file: " ++ err)
  exitFailure

-- | Display bytecode instructions and summary
displayBytecode :: Show a => [a] -> IO ()
displayBytecode bytecode = do
  putStrLnColored "\ESC[1m\ESC[32m" "Binary file decoded successfully"
  putStrLn ""
  putStrLnColored "\ESC[1m\ESC[33m" "BYTECODE:"
  mapM_ (putStrLn . ("  " ++) . show) bytecode
  putStrLn ""
  putStrLnColored "\ESC[1m\ESC[36m" "SUMMARY:"
  putStrLn $ "  Total instructions: " ++ show (length bytecode)
  exitSuccess

-- | Compile a source file through parse and compile phases
compileSourceFile :: FilePath -> CompileMode -> Bool -> IO ()
compileSourceFile filePath mode verbose = do
  putStrLnColored "\ESC[1m\ESC[36m" ("Reading file: " ++ filePath)
  content <- TIO.readFile filePath
  parseAndCompile filePath content mode verbose

-- | Parse source and compile to bytecode
parseAndCompile :: FilePath -> Text -> CompileMode -> Bool -> IO ()
parseAndCompile filePath content mode verbose = do
  putStrLn ""
  putStrLnColored "\ESC[1m\ESC[34m" "PARSING PHASE"
  putStrLn ""
  case parse pProgram filePath content of
    Left err -> handleParseError filePath content err
    Right ast -> compileAST filePath ast mode verbose

-- | Handle parse error with rich context
handleParseError :: FilePath -> Text
                 -> ParseErrorBundle Text Void -> IO ()
handleParseError filePath content err = do
  putStrLnColored "\ESC[1m\ESC[31m" "PARSE ERROR DETECTED"
  putStrLn ""
  let errMsgStr = errorBundlePretty err
      rich = makeRichError filePath errMsgStr content
  putStrLn $ formatRichError rich
  putStrLn ""
  exitFailure

-- | Compile AST to bytecode and handle output mode
compileAST :: FilePath -> Program -> CompileMode -> Bool -> IO ()
compileAST filePath ast mode verbose = do
  putStrLnColored "\ESC[1m\ESC[32m" "Parsing successful"
  putStrLn ""
  when verbose $ printAST ast
  compileAndOutput filePath ast mode

-- | Print AST in verbose mode
printAST :: Program -> IO ()
printAST ast = do
  putStrLnColored "\ESC[1m\ESC[33m" "AST:"
  print ast
  putStrLn ""

-- | Compile AST and handle output based on mode
compileAndOutput :: FilePath -> Program -> CompileMode -> IO ()
compileAndOutput filePath ast mode = do
  putStrLnColored "\ESC[1m\ESC[34m" "COMPILATION PHASE"
  let bytecode = compileProgram ast
  putStrLnColored "\ESC[1m\ESC[32m" "Compilation successful"
  handleOutputMode filePath bytecode mode

-- | Handle different output modes
handleOutputMode :: FilePath -> Bytecode -> CompileMode -> IO ()
handleOutputMode _ bytecode ShowBytecode =
  showBytecodeAndExit bytecode
handleOutputMode filePath bytecode (WriteBinary outputPath) =
  writeBinaryAndExit filePath outputPath bytecode
handleOutputMode _ _ InspectBinary = do
  putStrLnColored "\ESC[1m\ESC[31m"
    "Error: InspectBinary mode in wrong context"
  exitFailure

-- | Display bytecode and exit
showBytecodeAndExit :: Bytecode -> IO ()
showBytecodeAndExit bytecode = do
  putStrLn ""
  putStrLnColored "\ESC[1m\ESC[33m" "BYTECODE:"
  mapM_ print bytecode
  putStrLn ""
  exitSuccess

-- | Write binary file and display summary
writeBinaryAndExit :: FilePath -> FilePath -> Bytecode -> IO ()
writeBinaryAndExit inputPath outputPath bytecode = do
  writeBinaryFile outputPath bytecode
  putStrLn ""
  putStrLnColored "\ESC[1m\ESC[32m"
    ("✓ Binary file written: " ++ outputPath)
  putStrLn ""
  printCompilationSummary inputPath outputPath bytecode
  exitSuccess

-- | Print compilation summary
printCompilationSummary :: FilePath -> FilePath -> Bytecode -> IO ()
printCompilationSummary inputPath outputPath bytecode = do
  putStrLnColored "\ESC[1m\ESC[36m" "SUMMARY:"
  putStrLn $ "  Input file:    " ++ inputPath
  putStrLn $ "  Output file:   " ++ outputPath
  putStrLn $ "  Instructions:  " ++ show (length bytecode)
  putStrLn $ "  File size:     (binary format)"
  putStrLn ""

-- | Parse command-line arguments into compiler options
parseArgs :: [String] -> Either String CompilerOptions
parseArgs args =
  let (flags, files) = partition isFlag args
      verbose = hasVerboseFlag flags
      showBytecodeFl = "--show-bytecode" `elem` flags
      inspectFl = "--inspect" `elem` flags
      nonFlags = filter (not . isOutputFlag) files
  in buildOptions nonFlags showBytecodeFl inspectFl verbose args

-- | Check if argument is a flag
isFlag :: String -> Bool
isFlag arg = arg `elem` flagList
  where
    flagList = ["--verbose", "-v", "--show-bytecode",
                "--show-ast", "--inspect"]

-- | Check if argument is output-related flag
isOutputFlag :: String -> Bool
isOutputFlag arg = arg `elem` ["-o", "--output"]

-- | Check if verbose mode is enabled
hasVerboseFlag :: [String] -> Bool
hasVerboseFlag flags =
  any (`elem` flags) ["--verbose", "-v", "--show-ast"]

-- | Build compiler options from parsed arguments
buildOptions :: [String] -> Bool -> Bool -> Bool -> [String]
             -> Either String CompilerOptions
buildOptions [] _ _ _ _ = Left "No input file provided"
buildOptions [inputFile] _ True verbose _ =
  Right $ CompilerOptions inputFile InspectBinary verbose
buildOptions [inputFile] True False verbose _ =
  Right $ CompilerOptions inputFile ShowBytecode verbose
buildOptions [inputFile] False False verbose _ =
  Right $ autoDetectMode inputFile verbose
buildOptions _ _ _ verbose args =
  handleOutputFlag args verbose

-- | Auto-detect mode based on file extension
autoDetectMode :: FilePath -> Bool -> CompilerOptions
autoDetectMode inputFile verbose
  | takeExtension inputFile == ".rtbc" =
      CompilerOptions inputFile InspectBinary verbose
  | otherwise =
      let outputFile = replaceExtension inputFile ".rtbc"
      in CompilerOptions inputFile (WriteBinary outputFile) verbose

-- | Handle -o/--output flag arguments
handleOutputFlag :: [String] -> Bool -> Either String CompilerOptions
handleOutputFlag args verbose
  | "-o" `elem` args || "--output" `elem` args =
      case parseOutputArgs args of
        Just (input, output) ->
          Right $ CompilerOptions input (WriteBinary output) verbose
        Nothing -> Left "Invalid -o/--output usage"
  | otherwise = Left "Invalid arguments"

-- | Parse -o/--output arguments from command line
parseOutputArgs :: [String] -> Maybe (FilePath, FilePath)
parseOutputArgs = findOutput
  where
    findOutput [] = Nothing
    findOutput ("-o":out:rest) =
      Just (findInput (filter (`notElem` ["-o", out]) rest), out)
    findOutput ("--output":out:rest) =
      Just (findInput (filter (`notElem` ["--output", out]) rest), out)
    findOutput (_:rest) = findOutput rest

    findInput xs =
      case filter (not . isKnownFlag) xs of
        (i:_) -> i
        [] -> ""

    isKnownFlag x = x `elem`
      ["--verbose", "-v", "--show-bytecode", "--show-ast", "--inspect"]

-- | Partition list based on predicate
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p xs = (filter p xs, filter (not . p) xs)

-- | Main entry point for the compiler
main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left errMsg -> printUsageAndExit errMsg
    Right opts -> processFile opts

-- | Print usage information and exit with failure
printUsageAndExit :: String -> IO ()
printUsageAndExit errMsg = do
  putStrLnColored "\ESC[1m\ESC[31m" errMsg
  putStrLn ""
  printUsage
  exitFailure

-- | Print compiler usage information
printUsage :: IO ()
printUsage = do
  putStrLn "Usage:"
  putStrLn "  glados <input.rat>                  \
           \    # Compile to .rtbc file"
  putStrLn "  glados <input.rat> -o <output.rtbc> \
           \    # Compile to specific file"
  putStrLn "  glados <input.rat> --show-bytecode  \
           \    # Show bytecode without writing file"
  putStrLn "  glados <input.rat> --verbose        \
           \    # Show AST and verbose output"
  putStrLn "  glados <input.rat> --show-ast       \
           \    # Show AST (alias for --verbose)"
  putStrLn "  glados <file.rtbc>                  \
           \    # Inspect compiled binary file"
  putStrLn "  glados <file.rtbc> --inspect        \
           \    # Inspect binary file (explicit)"
