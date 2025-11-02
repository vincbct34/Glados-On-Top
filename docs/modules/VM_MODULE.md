# VM Module Documentation

**Project**: GLaDOS/Ratatouille
**Version**: 3.0.0
**Last Updated**: November 2025

---

## Overview

The Virtual Machine (VM) module executes Ratatouille bytecode with full support for the actor model, including process isolation, message passing, and pattern matching. The VM provides a stack-based execution environment with strong memory safety guarantees from the Haskell runtime.

### Location

```
src/Ratatouille/VM/
  ├── VM.hs           - Core VM types and state management
  ├── Interpreter.hs  - Bytecode instruction execution
  └── Runtime.hs      - Process management and message passing
```

### Key Features

- **Stack-Based Execution**: RISC-like instruction interpretation
- **Actor Model**: Process isolation with message passing
- **Memory Safety**: Haskell GC prevents use-after-free and double-free
- **STM Concurrency**: Software Transactional Memory for safe message queues
- **Pattern Matching**: Native support for receive blocks
- **State Management**: Per-process mutable state

---

## Architecture

### Execution Model

```
Binary (.rtbc) → Decoder → [Instruction] → Interpreter → Runtime Effects
                                                ↓
                                          Stack Machine
                                                ↓
                                    Process Scheduler → Message Queues
```

### VM State Machine

```
┌──────────────┐
│  Load Code   │
└──────┬───────┘
       │
       ▼
┌──────────────┐
│ Register     │  (Scan for DEFINE_PROCESS/FUNCTION)
│ Definitions  │
└──────┬───────┘
       │
       ▼
┌──────────────┐
│ Call main()  │
└──────┬───────┘
       │
       ▼
┌──────────────┐
│ Execute Loop │  ◄──┐
│  - Fetch     │     │
│  - Decode    │     │
│  - Execute   │     │
└──────┬───────┘     │
       │             │
       ├─────────────┘
       │
       ▼
┌──────────────┐
│    HALT      │
└──────────────┘
```

---

## VM.hs - Core VM Types

### VMState

```haskell
data VMState = VMState
  { vmStack :: [Value]                      -- Operand stack
  , vmGlobals :: Map Text Value             -- Global variables
  , vmLocals :: Map Text Value              -- Local variables (process-scoped)
  , vmPc :: Int                             -- Program counter
  , vmBytecode :: Bytecode                  -- Instruction stream
  , vmLabels :: Map Text Int                -- Jump targets
  , vmProcessDefs :: Map Text ProcessDef    -- Process templates
  , vmFunctionDefs :: Map Text FunctionDef  -- Function templates
  , vmProcesses :: TVar (Map Pid Process)   -- Running processes (STM)
  , vmNextPid :: TVar Pid                   -- PID allocator (STM)
  , vmCurrentPid :: Maybe Pid               -- Current process context
  , vmDebugMode :: Bool                     -- Enable debug output
  , vmBreakpoints :: [Int]                  -- Debug breakpoints
  , vmTraceEnabled :: Bool                  -- Instruction tracing
  }
```

**Key Components**:

- **Stack**: Head of list = top of stack (efficient push/pop)
- **Globals vs Locals**: Globals shared, locals per-process
- **STM TVars**: Thread-safe process map and PID allocation
- **Current PID**: Context for message operations

### VM Monad

```haskell
newtype VM a = VM { runVM :: ExceptT VMError (StateT VMState IO) a }
  deriving (Functor, Applicative, Monad, MonadState VMState,
            MonadError VMError, MonadIO)
```

**Monad Transformers**:
- `ExceptT`: Error handling (VMError)
- `StateT`: VM state threading
- `IO`: Side effects (message passing, I/O)

**Benefits**:
- Automatic error propagation
- Pure functional state updates
- Composable operations

### VMError

```haskell
data VMError
  = StackUnderflow              -- Not enough values on stack
  | TypeError String            -- Type mismatch
  | UndefinedVariable Text      -- Variable not found
  | UndefinedProcess Text       -- Process definition not found
  | InvalidJump Int             -- Jump target out of bounds
  | InvalidLabel Text           -- Label not found
  | DivisionByZero              -- Arithmetic error
  | ProcessError Text           -- Process-related error
  | PatternMatchFailed          -- No pattern matched
  | RuntimeError String         -- General runtime error
```

### Process Types

#### ProcessDef (Template)

```haskell
data ProcessDef = ProcessDef
  { procName :: Text
  , procParams :: [Text]
  , procBody :: Bytecode
  }
```

**Purpose**: Blueprint for process instances

#### Process (Runtime Instance)

```haskell
data Process = Process
  { processId :: Pid
  , processStack :: [Value]
  , processLocals :: Map Text Value
  , processState :: Value              -- Mutable per-process state
  , processMailbox :: TQueue Message   -- STM message queue
  , processPc :: Int
  , processBytecode :: Bytecode
  , processThreadId :: Maybe ThreadId
  }
```

**Key Features**:
- **Isolated Stack**: Each process has its own stack
- **Isolated Locals**: No shared variables between processes
- **Mailbox**: Lock-free message queue (STM TQueue)
- **State**: Mutable state accessed via GET_STATE/SET_STATE

#### Message

```haskell
data Message = Message
  { msgSender :: Pid      -- Sender process ID
  , msgContent :: Value   -- Message payload
  }
```

**Sender Tracking**: Automatic `sender` local variable in receive blocks

---

## Interpreter.hs - Instruction Execution

### Execution Loop

```haskell
executeLoop :: VMState -> IO VMState
executeLoop state =
  if vmPc state >= length (vmBytecode state)
    then return state
    else do
      let instr = vmBytecode state !! vmPc state
      (result, newState) <- executeVM state (executeInstruction instr)
      case result of
        Left err -> error (show err)
        Right _ -> executeLoop (newState { vmPc = vmPc newState + 1 })
```

**Flow**:
1. Check if PC is beyond bytecode
2. Fetch instruction at PC
3. Execute instruction (may modify state)
4. Handle errors or continue
5. Increment PC and loop

### Instruction Handlers

#### Stack Operations

```haskell
PUSH_INT n → pushStack (VInt n)
PUSH_STRING s → pushStack (VString s)
POP_N n → replicateM_ n (void popStack)
DUP → peekStack >>= pushStack
```

**Example Execution**:
```
Initial stack: []
PUSH_INT 10    → [VInt 10]
PUSH_INT 20    → [VInt 20, VInt 10]
DUP            → [VInt 20, VInt 20, VInt 10]
POP_N 1        → [VInt 20, VInt 10]
```

#### Arithmetic Operations

```haskell
ADD → do
  b <- popStack
  a <- popStack
  pushStack (addValues a b)
```

**Type Handling**:
- `VInt + VInt → VInt`
- `VFloat + VFloat → VFloat`
- `VInt + VFloat → VFloat` (automatic promotion)

#### Comparison Operations

```haskell
CMP_EQ → do
  b <- popStack
  a <- popStack
  pushStack (VBool (a == b))
```

**Supported Types**: All value types support equality

#### Variable Operations

```haskell
LOAD_LOCAL name → do
  val <- loadLocal name      -- Map.lookup in vmLocals
  pushStack val

STORE_LOCAL name → do
  val <- popStack
  storeLocal name val        -- Map.insert in vmLocals
```

**Scoping**:
- Locals checked first
- Fallback to globals
- Error if not found

#### Process State

```haskell
GET_STATE → do
  state <- getProcessState   -- Lookup current process, get processState
  pushStack state

SET_STATE → do
  newState <- popStack
  setProcessState newState   -- Update processState in process map
```

**Usage**:
```ratatouille
state = state + 1  // Compiles to: GET_STATE, PUSH_INT 1, ADD, SET_STATE
```

#### Pattern Matching

##### MATCH_ATOM

```haskell
MATCH_ATOM atom offset → do
  val <- peekStack         -- Don't pop yet (for fallthrough)
  case val of
    VAtom a | a == atom → popStack >> return ()
    _ → jump offset      -- Jump to next pattern
```

**Example**:
```ratatouille
receive {
  | :ping → :pong
  | :close → none
}
```

**Bytecode**:
```
WAIT_MESSAGE
DUP
MATCH_ATOM "ping" 3    // If not :ping, jump 3
  PUSH_ATOM "pong"
  JUMP 3                // Skip to end
DUP
MATCH_ATOM "close" 2
  PUSH_NONE
```

##### MATCH_TUPLE

```haskell
MATCH_TUPLE size offset → do
  val <- peekStack
  case val of
    VTuple elems | length elems == size → do
      popStack
      mapM_ pushStack (reverse elems)  -- Push elements for binding
    _ → jump offset
```

**Example**:
```ratatouille
receive {
  | (:add, x, y) → x + y
}
```

**Bytecode**:
```
WAIT_MESSAGE
MATCH_TUPLE 3 5       // Expect 3 elements, jump 5 if no match
MATCH_VAR "y"         // Pop and bind y
MATCH_VAR "x"         // Pop and bind x
MATCH_ATOM "add" 2    // Match first element
  LOAD_LOCAL "x"
  LOAD_LOCAL "y"
  ADD
```

#### Actor Model Operations

##### DEFINE_PROCESS

```haskell
DEFINE_PROCESS name params body → do
  let pdef = ProcessDef name params body
  defineProcess pdef     // Store in vmProcessDefs
```

##### CREATE_INSTANCE (spawn)

```haskell
CREATE_INSTANCE name argCount → do
  args <- popStackN argCount
  pid <- createProcessInstance name (reverse args)
  pushStack (VPid $ fromPid pid)
```

**Process Creation**:
1. Allocate new PID
2. Create mailbox (TQueue)
3. Create Process record
4. Store in vmProcesses (TVar)
5. Return PID

##### SEND

```haskell
SEND → do
  msg <- popStack
  receiver <- popStack >>= toPid
  sender <- getCurrentPid
  sendMessage receiver msg
```

**Message Delivery**:
1. Create Message record (sender, content)
2. Write to receiver's mailbox (atomically via STM)
3. Receiver process scheduled for execution

##### WAIT_MESSAGE

```haskell
WAIT_MESSAGE → do
  (msg, senderPid) <- waitMessageWithSender
  storeLocal "sender" (VPid $ fromPid senderPid)  -- Automatic sender binding
  pushStack msg
```

**Blocking**: If no message, throws error (process suspends)

---

## Runtime.hs - Process Management

### Process Lifecycle

#### 1. Creation

```haskell
createProcessInstance :: Text → [Value] → VM Pid
createProcessInstance name args = do
  pdef <- getProcessDef name
  pid <- allocatePid
  mailbox <- liftIO $ atomically newTQueue
  let process = Process
        { processId = pid
        , processStack = args
        , processLocals = Map.empty
        , processState = VNone
        , processMailbox = mailbox
        , processPc = 0
        , processBytecode = procBody pdef
        , processThreadId = Nothing
        }
  processesVar <- gets vmProcesses
  liftIO $ atomically $ modifyTVar processesVar (Map.insert pid process)
  return pid
```

#### 2. Execution

**Synchronous Model** (current):
- Process runs when it receives a message
- Main process calls `executeReceivingProcess`
- Process bytecode executed in current thread

**Future: Asynchronous Model**:
- Each process runs in its own Haskell thread
- Concurrent execution
- Scheduler manages CPU allocation

#### 3. Message Handling

```haskell
sendMessage :: Pid → Value → VM ()
sendMessage targetPid msg = do
  senderPid <- getCurrentPid
  process <- lookupProcess targetPid
  let message = Message senderPid msg
  liftIO $ atomically $ writeTQueue (processMailbox process) message
```

**STM Benefits**:
- Atomic message delivery
- No race conditions
- Composable operations

#### 4. Termination

```haskell
exitCurrentProcess :: VM ()
exitCurrentProcess = do
  pid <- getCurrentPid
  processesVar <- gets vmProcesses
  liftIO $ atomically $ modifyTVar processesVar (Map.delete pid)
```

### State Management

#### GET_STATE / SET_STATE

**Implementation**:
```haskell
getProcessState :: VM Value
getProcessState = do
  pid <- getCurrentPid
  process <- lookupProcess pid
  return (processState process)

setProcessState :: Value → VM ()
setProcessState newState = do
  pid <- getCurrentPid
  processesVar <- gets vmProcesses
  liftIO $ atomically $ modifyTVar processesVar $
    Map.adjust (\p → p { processState = newState }) pid
```

**Usage**:
```ratatouille
proc Counter(init) {
  state: init,     // INIT_STATE
  receive {
    | :inc → state = state + 1  // GET_STATE, ADD, SET_STATE
  }
}
```

---

## Concurrency Model

### Current: Cooperative Multitasking

**Strategy**:
- Single-threaded execution
- Process runs to completion or WAIT_MESSAGE
- Explicit scheduling by main process

**Benefits**:
- Simple implementation
- Predictable execution
- No preemption issues

**Limitations**:
- No true parallelism
- Long-running process blocks others

### Future: Preemptive Multitasking

**Strategy**:
- Each process = Haskell lightweight thread
- GHC runtime handles scheduling
- Automatic load balancing

**Implementation**:
```haskell
runProcessThread :: Pid → VMState → IO ()
runProcessThread pid state = forkIO $ do
  loop state
  where
    loop st = do
      msg <- atomically $ readTQueue mailbox
      newState <- executeReceiveBlock msg st
      loop newState
```

---

## Memory Safety

### Guaranteed by Haskell Runtime

1. **No Use-After-Free**: GC tracks all references
2. **No Double-Free**: GC manages memory
3. **No Buffer Overflows**: Array bounds checked
4. **No Null Pointer Dereference**: Maybe/Either types

### Process Isolation

**Enforced by VM**:
- No shared stack access
- No shared locals access
- Message passing only

**Example**:
```ratatouille
proc A() { let x = 10 }
proc B() { x }  // ERROR: x not in scope
```

---

## Performance Characteristics

| Operation | Time | Space | Notes |
|-----------|------|-------|-------|
| Stack push/pop | O(1) | O(1) | List cons/decons |
| Variable lookup | O(log n) | O(1) | Map.lookup |
| Message send | O(1) amortized | O(msg) | STM TQueue |
| Process spawn | O(1) | O(code) | Allocate + insert |
| Pattern match | O(patterns) | O(1) | Sequential |
| Instruction fetch | O(1) | O(1) | Array index |

### Optimization Opportunities

1. **Bytecode Caching**: Keep decoded bytecode in memory
2. **JIT Compilation**: Hot path detection + native code
3. **Inline Functions**: Small functions expanded at call site
4. **Tail Call Optimization**: Convert recursion to loops

---

## Debug Features

### Trace Mode

```haskell
traceInstruction :: Instruction → VM ()
traceInstruction instr = do
  enabled <- isTraceEnabled
  when enabled $ do
    pc <- getPc
    stack <- gets vmStack
    liftIO $ putStrLn $ "[TRACE] PC=" ++ show pc ++
                        " | " ++ show instr ++
                        " | Stack=" ++ show stack
```

**Output**:
```
[TRACE] PC=0 | PUSH_INT 10 | Stack=[]
[TRACE] PC=1 | PUSH_INT 20 | Stack=[VInt 10]
[TRACE] PC=2 | ADD | Stack=[VInt 20, VInt 10]
[TRACE] PC=3 | STORE_VAR "result" | Stack=[VInt 30]
```

### Breakpoints

```haskell
checkBreakpoint :: VM Bool
checkBreakpoint = do
  pc <- getPc
  breakpoints <- gets vmBreakpoints
  return $ pc `elem` breakpoints
```

**Usage**: Set breakpoints at specific instruction indices

---

## Best Practices

### 1. Process Design

**Keep processes small and focused**:
```ratatouille
// Good: Single responsibility
proc Counter(init) { ... }
proc Logger(file) { ... }

// Bad: Too many responsibilities
proc EverythingManager() { ... }
```

### 2. Message Protocols

**Use atoms for message tags**:
```ratatouille
// Good: Clear intent
pid <- (:increment, 1)
pid <- (:decrement, 1)

// Bad: Ambiguous
pid <- (1, 1)
```

### 3. State Management

**Initialize state explicitly**:
```ratatouille
proc Counter(init) {
  state: init,  // Clear initialization
  receive { ... }
}
```

### 4. Error Handling

**Always include catch-all pattern**:
```ratatouille
receive {
  | :expected1 → ...
  | :expected2 → ...
  | _ → none  // Handle unexpected messages
}
```

---

## Related Documentation

- [Bytecode Module](BYTECODE_MODULE.md) - Bytecode → VM
- [Architecture Overview](ARCHITECTURE.md) - System design
- [Parser Module](PARSER_MODULE.md) - Source → AST
- [AST Module](AST_MODULE.md) - AST structure

---

**Document Version**: 1.0
**Maintainers**: GLaDOS Development Team
