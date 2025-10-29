{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- VM entry point - Execute bytecode files
-}

module Main (main) where

import Control.Concurrent.STM (newTVarIO)
import qualified Data.Map as Map
import Ratatouille.Bytecode.Decoder (readBinaryFile)
import Ratatouille.Bytecode (Instruction(..), Value(..), Bytecode)
import qualified Ratatouille.Bytecode.Types as BT
import Ratatouille.VM.Interpreter (executeBytecode, startREPL)
import Ratatouille.VM.VM (VMState (..), executeVM)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Text.Read (readMaybe)

-- | Convert from Types.Instruction to Bytecode.Instruction
convertInstruction :: BT.Instruction -> Instruction
convertInstruction instr = case instr of
  BT.PUSH_INT n -> PUSH_INT n
  BT.PUSH_STRING s -> PUSH_STRING s
  BT.PUSH_ATOM a -> PUSH_ATOM a
  BT.PUSH_UNIT -> PUSH_UNIT
  BT.PUSH_NONE -> PUSH_NONE
  BT.PUSH_BOOL b -> PUSH_BOOL b
  BT.PUSH_TUPLE n -> PUSH_TUPLE n
  BT.LOAD_VAR name -> LOAD_VAR name
  BT.STORE_VAR name -> STORE_VAR name
  BT.LOAD_LOCAL name -> LOAD_LOCAL name
  BT.STORE_LOCAL name -> STORE_LOCAL name
  BT.INIT_STATE -> INIT_STATE
  BT.GET_STATE -> GET_STATE
  BT.SET_STATE -> SET_STATE
  BT.ADD -> ADD
  BT.SUB -> SUB
  BT.MUL -> MUL
  BT.DIV -> DIV
  BT.CONCAT -> CONCAT
  BT.CMP_EQ -> CMP_EQ
  BT.CMP_NEQ -> CMP_NEQ
  BT.CMP_LT -> CMP_LT
  BT.CMP_GT -> CMP_GT
  BT.CMP_LTE -> CMP_LTE
  BT.CMP_GTE -> CMP_GTE
  BT.LOGIC_AND -> LOGIC_AND
  BT.LOGIC_OR -> LOGIC_OR
  BT.GET_FIELD name -> GET_FIELD name
  BT.DEFINE_PROCESS name params body -> DEFINE_PROCESS name params (map convertInstruction body)
  BT.CREATE_INSTANCE name -> CREATE_INSTANCE name
  BT.SEND -> SEND
  BT.WAIT_MESSAGE -> WAIT_MESSAGE
  BT.MATCH_ATOM atom offset -> MATCH_ATOM atom offset
  BT.MATCH_VAR name -> MATCH_VAR name
  BT.MATCH_TUPLE size offset -> MATCH_TUPLE size offset
  BT.MATCH_WILDCARD -> MATCH_WILDCARD
  BT.PROCESS_LOOP -> PROCESS_LOOP
  BT.SELF -> SELF
  BT.EXIT_PROCESS -> EXIT_PROCESS
  BT.JUMP offset -> JUMP offset
  BT.JUMP_IF_FALSE offset -> JUMP_IF_FALSE offset
  BT.LABEL name -> LABEL name
  BT.CALL name -> CALL name
  BT.RETURN -> RETURN
  BT.HALT -> HALT
  -- For instructions not in the original Bytecode module, use HALT as fallback
  _ -> HALT

-- | Convert bytecode from Types to Bytecode
convertBytecode :: BT.Bytecode -> Bytecode
convertBytecode = map convertInstruction

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      printUsage
      exitSuccess

    ["--help"] -> do
      printUsage
      exitSuccess

    ["--repl"] -> do
      initialState <- createInitialVMState []
      _ <- executeVM initialState startREPL
      exitSuccess

    ["--debug"] -> do
      putStrLn "Error: --debug requires a bytecode file"
      exitFailure

    ["--trace"] -> do
      putStrLn "Error: --trace requires a bytecode file"
      exitFailure

    ["--debug", file] -> do
      runFileWithOptions file True False
      exitSuccess

    ["--trace", file] -> do
      runFileWithOptions file False True
      exitSuccess

    [file] -> do
      runFile file
      exitSuccess

    _ -> do
      putStrLn "Error: Invalid arguments"
      printUsage
      exitFailure

bold :: String -> String
bold str = "\ESC[1m" ++ str ++ "\ESC[0m"

red :: String -> String
red str = "\ESC[31m" ++ str ++ "\ESC[0m"

green :: String -> String
green str = "\ESC[32m" ++ str ++ "\ESC[0m"

blue :: String -> String
blue str = "\ESC[34m" ++ str ++ "\ESC[0m"

runBytecodeFile :: FilePath -> IO ()
runBytecodeFile filePath = do
  putStrLn $ (bold "Loading bytecode from: ") ++ filePath
  content <- readFile filePath
  putStrLn (bold "\n=== Parsing Bytecode ===")
  case parseInstructions (lines content) of
    Nothing -> do
      putStrLn (bold (red "Error: Invalid bytecode format"))
      exitFailure
    Just instructions -> do
      putStrLn (bold (green "[OK] Bytecode loaded successfully!"))
      putStrLn $ bold "Total instructions: " ++ show (length instructions)
      putStrLn (bold "\n=== Execution Phase ===")
      initialState <- createInitialVMState instructions
      (result, finalState) <- executeVM initialState (executeBytecode instructions)
      putStrLn (bold "\n=== Execution Result ===")
      case result of
        Left err -> do
          putStrLn $ (bold (red "Runtime Error: ")) ++ show err
          exitFailure
        Right value -> do
          putStrLn $ "Result: " ++ show value
          putStrLn (bold "\n=== Final VM State ===")
          putStrLn $ "Stack: " ++ show (vmStack finalState)
          putStrLn $ "Globals: " ++ show (vmGlobals finalState)
          exitSuccess

-- Create initial VM state
createInitialVMState :: [Instruction] -> IO VMState
createInitialVMState bytecode = do
  processesVar <- newTVarIO Map.empty
  nextPidVar <- newTVarIO 1
  return
    VMState
      { vmStack = [],
        vmGlobals = Map.empty,
        vmLocals = Map.empty,
        vmPc = 0,
        vmBytecode = bytecode,
        vmLabels = Map.empty,
        vmProcessDefs = Map.empty,
        vmProcesses = processesVar,
        vmNextPid = nextPidVar,
        vmCurrentPid = Nothing,
        vmDebugMode = False,
        vmBreakpoints = [],
        vmTraceEnabled = False
      }

-- Parse bytecode instructions from string representation
parseInstructions :: [String] -> Maybe [Instruction]
parseInstructions = mapM (readMaybe . trim)
  where
    trim = dropWhile (== ' ')

-- | Print usage information
printUsage :: IO ()
printUsage = putStrLn $ unlines
  [ "GLaDOS Virtual Machine"
  , ""
  , "Usage: glados-vm [OPTIONS] [FILE]"
  , ""
  , "Options:"
  , "  --repl           Start interactive REPL mode"
  , "  --debug FILE     Run FILE with debug mode enabled"
  , "  --trace FILE     Run FILE with instruction tracing"
  , "  --help           Show this help message"
  , ""
  , "Examples:"
  , "  glados-vm program.bc          # Run bytecode from file"
  , "  glados-vm --repl              # Start REPL"
  , "  glados-vm --debug program.bc  # Run with debugging"
  , "  glados-vm --trace program.bc  # Run with tracing"
  , ""
  , "In the current version, bytecode must be provided as a Haskell"
  , "data structure. File loading support coming soon."
  ]

-- | Run bytecode from a file
runFile :: FilePath -> IO ()
runFile file = runFileWithOptions file False False

-- | Run bytecode from a file with options
runFileWithOptions :: FilePath -> Bool -> Bool -> IO ()
runFileWithOptions file debugMode traceMode = do
  putStrLn $ "Loading bytecode from: " ++ file
  bytecodeResult <- readBinaryFile file
  case bytecodeResult of
    Left err -> do
      putStrLn $ "Error reading bytecode: " ++ err
      exitFailure
    Right bytecode -> do
      let convertedBytecode = convertBytecode bytecode
      vmState <- createInitialVMState convertedBytecode
      (result, finalState) <- executeVM vmState (executeBytecode convertedBytecode)
      case result of
        Left err -> do
          putStrLn $ "Error: " ++ show err
          exitFailure
        Right value -> do
          putStrLn $ "Program completed successfully"
          putStrLn $ "Result: " ++ show value
          exitSuccess
