{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- VM entry point - Execute bytecode files
-}

module Main (main) where

import Control.Concurrent.STM (atomically, newTQueue, newTVarIO, modifyTVar)
import qualified Data.Map as Map
import Data.List (isInfixOf)
import qualified Data.Text as T
import Ratatouille.Bytecode.Decoder (readBinaryFile)
import Ratatouille.Bytecode.Types (Instruction(..), Value(..), Bytecode)
import Ratatouille.VM.Interpreter (executeBytecode)
import Ratatouille.VM.VM (VMState (..), VMError(..), executeVM, Process(..), Pid(..))
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)


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
      putStrLn "REPL mode not available in this version"
      exitFailure

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


-- Create initial VM state
createInitialVMState :: Bytecode -> IO VMState
createInitialVMState bytecode = do
  processesVar <- newTVarIO Map.empty
  nextPidVar <- newTVarIO 1
  
  -- Create main process (Pid 0) with a mailbox
  mainMailbox <- atomically newTQueue
  let mainProcess = Process
        { processId = Pid 0
        , processStack = []
        , processLocals = Map.empty
        , processState = VUnit
        , processMailbox = mainMailbox
        , processPc = 0
        , processBytecode = bytecode
        , processThreadId = Nothing
        }
  
  -- Insert main process into process map
  atomically $ modifyTVar processesVar (Map.insert (Pid 0) mainProcess)
  
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
        vmCurrentPid = Just (Pid 0),  -- Main process with mailbox
        vmDebugMode = False,
        vmBreakpoints = [],
        vmTraceEnabled = False
      }


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
  bytecodeResult <- readBinaryFile file
  case bytecodeResult of
    Left err -> do
      putStrLn $ "Error reading bytecode: " ++ err
      exitFailure
    Right bytecode -> do
      vmState <- createInitialVMState bytecode
      let vmState' = vmState { vmDebugMode = debugMode, vmTraceEnabled = traceMode }
      (result, finalState) <- executeVM vmState' (executeBytecode bytecode)
      case result of
        Left (ProcessError msg) | "No message in mailbox" `isInfixOf` (T.unpack msg) -> do
          -- Process terminated normally after handling all messages
          exitSuccess
        Left err -> do
          putStrLn $ "Error: " ++ show err
          exitFailure
        Right _value -> do
          -- Program completed successfully - exit silently
          exitSuccess
