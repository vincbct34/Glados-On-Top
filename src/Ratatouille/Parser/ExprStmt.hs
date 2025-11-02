{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Expression and statement parsers (REFACTORED & OPTIMIZED)
--
-- This module implements expression parsing with proper operator precedence.
--
-- OPERATOR PRECEDENCE (lowest to highest):
--   1. Assignment        =        (right-associative: a = b = c → a = (b = c))
--   2. Send             <-        (right-associative: a <- b <- c → a <- (b <- c))
--   3. Logical OR       ||        (left-associative)
--   4. Logical AND      &&        (left-associative)
--   5. Comparison       ==, !=, <, >, <=, >=  (left-associative)
--   6. Additive         +, -, ++  (left-associative)
--   7. Multiplicative   *, /      (left-associative)
--   8. Postfix          .field    (left-associative: a.b.c → (a.b).c)
--   9. Base expressions           (literals, variables, calls, etc.)
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Ratatouille.Parser.ExprStmt
  ( -- * Main Expression Parser
    pExpr
  , pBlock
    -- * Statement Parsers
  , pStatement
  , pTopLevelStatement
  , pLet
  , pLetDestructure
  , pAssign
    -- * Precedence Level Parsers (for testing/reuse)
  , pExprAssign
  , pExprSend
  , pExprAdditive
  , pExprMultiplicative
    -- * Operator Parsers (for testing/reuse)
  , pOpMulDiv
  , pOpAddSub
    -- * Special Expression Parsers
  , pReceive
  , pMatch
  )
where

import Data.Functor (void)
import Data.Text (Text)
import Ratatouille.AST
  ( CastType (..)
  , Expr (..)
  , Literal (..)
  , MatchCase (..)
  , Op (..)
  , Pattern (..)
  , ReceiveCase (..)
  , Stmt (..)
  , Type (..)
  , UnaryOp (..)
  )
import Ratatouille.Parser.Common
  ( Parser
  , pAtom
  , pIdentifier
  , pLiteral
  , pType
  , symbol
  )
import Text.Megaparsec
  ( MonadParsec (notFollowedBy, try)
  , between
  , choice
  , many
  , optional
  , satisfy
  , sepBy
  , sepEndBy
  , (<|>)
  )
import Text.Megaparsec.Char (char)

-- =============================================================================
-- PUBLIC API
-- =============================================================================

-- | Main entry point: parse any expression
-- Starts at the lowest precedence level (assignment)
pExpr :: Parser Expr
pExpr = pExprAssign

-- | Parse any statement (let, let const, assign, or expression as statement)
-- Note: "let const" is parsed by pLet, there is no standalone "const" statement
pStatement :: Parser Stmt
pStatement = choice [try pLetDestructure, try pLet, pAssign, SExpr <$> pExpr]

-- | Parse top-level statement (excludes const - only let, assign, or expression)
-- Constants are only allowed inside blocks/expressions, not at the top level
pTopLevelStatement :: Parser Stmt
pTopLevelStatement = choice [try pLetDestructure, try pLet, pAssign, SExpr <$> pExpr]


-- =============================================================================
-- EXPRESSION PRECEDENCE CHAIN (Level 1-8: Lowest to Highest)
-- =============================================================================
-- Each level calls the next higher precedence level, creating the correct
-- operator precedence automatically. Lower precedence = evaluated later.

-- | LEVEL 1: Assignment (a = b)
-- Right-associative: a = b = c means a = (b = c)
pExprAssign :: Parser Expr
pExprAssign = try pAssignment <|> pExprSend
  where
    pAssignment = EAssign <$> pIdentifier <* symbol "=" <*> pExprAssign

-- | LEVEL 2: Send operator (receiver <- message)
-- Right-associative: sends can be chained
pExprSend :: Parser Expr
pExprSend = do
  receiver <- pExprLogicalOr
  optional (try (symbol "<-")) >>= \case
    Nothing -> pure receiver
    Just _ -> ESend receiver <$> pExprSend

-- | LEVEL 3: Logical OR (a || b)
-- Left-associative: a || b || c means (a || b) || c
pExprLogicalOr :: Parser Expr
pExprLogicalOr = chainLeft pExprLogicalAnd pOpLogicalOr

-- | LEVEL 4: Logical AND (a && b)
-- Left-associative: a && b && c means (a && b) && c
pExprLogicalAnd :: Parser Expr
pExprLogicalAnd = chainLeft pExprComparison pOpLogicalAnd

-- | LEVEL 5: Comparison (==, !=, <, >, <=, >=)
-- Left-associative
pExprComparison :: Parser Expr
pExprComparison = chainLeft pExprAdditive pOpComparison

-- | LEVEL 6: Additive (+, -, ++)
-- Left-associative: 1 + 2 + 3 means (1 + 2) + 3
pExprAdditive :: Parser Expr
pExprAdditive = chainLeft pExprMultiplicative pOpAddSub

-- | LEVEL 7: Multiplicative (*, /)
-- Left-associative: 2 * 3 * 4 means (2 * 3) * 4
pExprMultiplicative :: Parser Expr
pExprMultiplicative = chainLeft pUnaryExpr pOpMulDiv

-- | LEVEL 7.5: Unary operators (!, -, +)
-- Right-associative: !!x means !(!(x))
pUnaryExpr :: Parser Expr
pUnaryExpr = (EUnaryOp <$> pUnaryOp <*> pUnaryExpr) <|> pPostfixExpr
  where
    pUnaryOp = choice
      [ UNot <$ symbol "!",
        try (UNeg <$ (symbol "-" <* notFollowedBy (satisfy isDigit))),
        UPlus <$ symbol "+"
      ]
    isDigit c = c >= '0' && c <= '9'

-- | LEVEL 8: Postfix (field access and array indexing)
-- Left-associative: a.b.c means (a.b).c, arr[0][1] means (arr[0])[1]
pPostfixExpr :: Parser Expr
pPostfixExpr = do
  base <- pBaseExpr
  postfixes <- many (try pFieldAccess <|> pArrayIndex)
  return $ foldl' (\expr postfix -> postfix expr) base postfixes
  where
    pFieldAccess = (\fieldName expr -> EFieldAccess expr fieldName) <$> (symbol "." *> pIdentifier)
    pArrayIndex = (\idx expr -> EIndex expr idx) <$> between (symbol "[") (symbol "]") pExpr


-- =============================================================================
-- OPERATOR PARSERS (Alphabetically Sorted)
-- =============================================================================
-- These parsers recognize specific operators and return their corresponding
-- Op constructors. They are used by the precedence chain parsers above.

-- | Additive operators: +, -, and ++ (string concatenation)
--
-- Note: ++ must be tried before + to avoid parsing ++ as two + operators
--
-- Examples:
--   "+"  → Add
--   "-"  → Sub
--   "++" → Concat
pOpAddSub :: Parser Op
pOpAddSub = choice
  [ try (Concat <$ symbol "++"),
    Add <$ symbol "+",
    Sub <$ symbol "-"
  ]

-- | Comparison operators: ==, !=, <, >, <=, >=
--
-- Note: Multi-character operators must be tried first (<=, >=, ==, !=)
-- Special case: < must not match <- (send operator)
--
-- Examples:
--   "==" → Eq
--   "!=" → Neq
--   "<"  → Lt (but not followed by -)
--   ">"  → Gt
--   "<=" → Lte
--   ">=" → Gte
pOpComparison :: Parser Op
pOpComparison = choice
  [ try (Lte <$ symbol "<="),
    try (Gte <$ symbol ">="),
    try (Eq <$ symbol "=="),
    try (Neq <$ symbol "!="),
    try (Lt <$ (symbol "<" <* notFollowedBy (char '-'))),
    Gt <$ symbol ">"
  ]

-- | Logical AND operator: &&
--
-- Examples:
--   "&&" → And
pOpLogicalAnd :: Parser Op
pOpLogicalAnd = And <$ symbol "&&"

-- | Logical OR operator: ||
--
-- Examples:
--   "||" → Or
pOpLogicalOr :: Parser Op
pOpLogicalOr = Or <$ symbol "||"

-- | Multiplicative operators: * and /
--
-- Examples:
--   "*" → Mul
--   "/" → Div
pOpMulDiv :: Parser Op
pOpMulDiv = choice [Mul <$ symbol "*", Div <$ symbol "/"]


-- =============================================================================
-- BASE EXPRESSIONS (Level 9: Highest Precedence)
-- =============================================================================

-- | Base expressions: literals, variables, keywords, parentheses, etc.
--
-- These are the "atoms" that can't be broken down further by operator
-- precedence rules. They form the foundation of the expression hierarchy.
--
-- Includes:
--   - Literals: 42, "hello", none
--   - Atoms: :ok, :error
--   - Keywords: spawn, if, receive, match, self, scast, rcast
--   - Variables and function calls
--   - Braces (blocks and tuples)
--   - Parentheses (grouping)
pBaseExpr :: Parser Expr
pBaseExpr = choice
  [ ELiteral <$> pLiteral,        -- 42, "hello", none
    pAtom,                          -- :ok, :error
    try pSpawn,                     -- spawn Process(args)
    try pIf,                        -- if cond then expr else expr
    try pReceive,                   -- receive { | pattern -> expr }
    try pMatch,                     -- match expr { | pattern -> expr }
    try pCast,                      -- scast<type>(expr), rcast<type>(expr)
    try pMaybeEither,               -- Just(x), Nothing, Left(x), Right(x)
    pSelf,                          -- self
    try pArrayOrIndex,              -- [1, 2, 3] or arr[idx]
    try pPreIncDec,                 -- ++x, --x
    pVarOrCall,                     -- foo or foo(x, y) or x++, x--
    pBraceContent,                  -- { block } or { tuple }
    pParens                         -- ( expr )
  ]


-- =============================================================================
-- SPECIFIC EXPRESSION PARSERS (Alphabetically Sorted)
-- =============================================================================
-- These parsers handle specific expression types that appear in pBaseExpr.
-- All parsers in this section are alphabetically sorted for easy maintenance.

-- | Parse function argument list: (expr1, expr2, ...)
--
-- Used by function calls and spawn expressions.
-- Arguments are comma-separated and may have trailing commas.
--
-- Examples:
--   ()          → []
--   (42)        → [ELiteral 42]
--   (1, 2, 3)   → [ELiteral 1, ELiteral 2, ELiteral 3]
--   (x, y,)     → [EVar "x", EVar "y"] (trailing comma allowed)
pArgumentList :: Parser [Expr]
pArgumentList = parens (sepEndBy pExpr comma)
  where
    parens = between (symbol "(") (symbol ")")
    comma = symbol ","

-- | Parse a block: { statements final_expr }
--
-- A block contains one or more statements, optionally followed by a final
-- expression. The value of a block is the value of its final expression,
-- or 0 if there is none.
--
-- Syntax rules:
--   - Statements are separated by newlines/whitespace (no semicolons)
--   - The final expression is optional
--   - A let statement at the end is also valid
--
-- Examples:
--   { let x = 5  x + 1 }           → EBlock [SLet "x" 5] (x + 1)
--   { let x = 5  let y = 10  x + y } → EBlock [SLet "x" 5, SLet "y" 10] (x + y)
--   { x = x + 1  x }               → EBlock [SAssign "x" (x + 1)] x
--   { let x = 5 }                  → EBlock [SLet "x" 5] 0
pBlock :: Parser Expr
pBlock = do
  -- Try to parse statements/let, but also allow pure expressions
  firstItem <- try (Left <$> pStatement) <|> try (Right <$> pLet) <|> (Right <$> (SExpr <$> pExpr))
  case firstItem of
    Left firstStmt -> parseRestOfBlock [firstStmt]
    Right firstStmtOrLet -> parseRestOfBlock [firstStmtOrLet]

-- | Braces: { } for blocks only
--
-- Disambiguation strategy:
--   - Braces are now ONLY for blocks
--   - Tuples use parentheses ()
--
-- Block examples:
--   {let x = 5  x}  → block (let followed by expr)
--   {x = x + 1  x}  → block (assignment followed by expr)
pBraceContent :: Parser Expr
pBraceContent = braces pBlock
  where
    braces = between (symbol "{") (symbol "}")

-- | Cast expression: type conversion
--
-- Ratatouille provides two types of casts:
--   - scast (static cast): Safe conversion with validation
--   - rcast (reinterpret cast): Unsafe reinterpretation of bits
--   - ccast (const cast): Remove const qualification
--
-- Format: castfunc<targettype>(expression)
--         castfunc(expression)  (for const cast, no type needed)
--
-- Static Cast (scast):
--   Performs safe type conversions with validation:
--   - Numeric widening (i8 → i32, i32 → i64, etc.)
--   - Numeric narrowing with overflow checking
--   - Float ↔ Integer conversions
--   - Will cause runtime error if conversion is invalid
--
-- Reinterpret Cast (rcast):
--   Reinterprets the bit pattern as a different type:
--   - No validation or conversion
--   - Directly treats the bits as the target type
--   - Unsafe: can cause undefined behavior
--   - Use with extreme caution
--
-- Const Cast (ccast):
--   Removes const qualification from a variable:
--   - Allows modification of const variables
--   - No type parameter needed
--   - Use to bypass const safety when necessary
--   - Should be used sparingly
--
-- Examples:
--   scast<i64>(42)           → ECast StaticCast (TNumeric I64) (ELiteral 42)
--   scast<f32>(100)          → ECast StaticCast (TNumeric F32) (ELiteral 100)
--   scast<i8>(x)             → ECast StaticCast (TNumeric I8) (EVar "x")
--   rcast<u32>(-1)           → ECast ReinterpretCast (TNumeric U32) (ELiteral -1)
--   rcast<f32>(bits)         → ECast ReinterpretCast (TNumeric F32) (EVar "bits")
--   ccast(const_var)         → ECast ConstCast TAny (EVar "const_var")
--
-- Type Safety:
--   - scast: Checked at runtime, throws error on invalid conversion
--   - rcast: No checks, assumes programmer knows what they're doing
--   - ccast: Removes const, allows modification of immutable variables
pCast :: Parser Expr
pCast = do
  castType <- choice
    [ StaticCast <$ symbol "scast",
      ReinterpretCast <$ symbol "rcast",
      ConstCast <$ symbol "ccast"
    ]
  case castType of
    ConstCast -> do
      -- Const cast doesn't need a type parameter
      expr <- between (symbol "(") (symbol ")") pExpr
      return $ ECast ConstCast TAny expr
    _ -> do
      -- Static and reinterpret casts need type parameter
      targetType <- between (symbol "<") (symbol ">") pType
      expr <- between (symbol "(") (symbol ")") pExpr
      return $ ECast castType targetType expr

-- | If-then-else expression
--
-- The else branch is optional. If omitted, the expression evaluates to
-- none when the condition is false.
--
-- Format: if condition then expr else expr
--
-- Examples:
--   if x > 0 then 1 else -1    → EIf (x > 0) 1 (Just -1)
--   if ready then start()      → EIf ready (start()) Nothing
--   if true then {let x = 5; x} else {let y = 10; y}
pIf :: Parser Expr
pIf = EIf
  <$> (symbol "if" *> pExpr)
  <*> (symbol "then" *> pExpr)
  <*> optional (symbol "else" *> pExpr)

-- | Maybe and Either constructors
--
-- Ratatouille supports functional-style optional and error-handling types:
--   - Maybe T:  represents an optional value (T or none)
--   - Either T U: represents a value that can be one of two types
--
-- Constructors:
--   - just(expr):    wraps a value in Maybe (present/has value)
--   - none:          represents absence of value in Maybe (no value)
--   - ok(expr):      wraps a value in the right side of Either (success case)
--   - ko(expr):      wraps a value in the left side of Either (error case)
--
-- Format:
--   just(expression)
--   none
--   ok(expression)
--   ko(expression)
--
-- Examples:
--   just(42)              → EJust (ELiteral 42)
--   just("hello")         → EJust (ELiteral "hello")
--   none                  → ENone
--   ko(:not_found)        → ELeft (EAtom "not_found")
--   ok(result)            → ERight (EVar "result")
--   just(x + y)           → EJust (EBinOp Add (EVar "x") (EVar "y"))
--
-- Note: 'none' serves double duty: as a null literal AND as the Maybe constructor for absence
pMaybeEither :: Parser Expr
pMaybeEither = choice
  [ EJust <$> (symbol "just" *> between (symbol "(") (symbol ")") pExpr),
    ENone <$ symbol "none",
    ELeft <$> (symbol "ko" *> between (symbol "(") (symbol ")") pExpr),
    ERight <$> (symbol "ok" *> between (symbol "(") (symbol ")") pExpr)
  ]

-- | Parenthesized expression or tuple: (expr) or (expr1, expr2, ...)
--
-- Used for:
--   - Grouping to override operator precedence: (1 + 2) * 3
--   - Tuples with comma-separated expressions: (1, 2, 3)
--
-- Examples:
--   (1 + 2) * 3       → (1 + 2) is evaluated first
--   (x)               → same as x, but explicitly grouped
--   ((a + b) * c)     → nested grouping
--   (1, 2, 3)         → ETuple [1, 2, 3] (tuple - at least 2 elements)
--   (x, y)            → ETuple [x, y]
-- Note: Empty tuples () are not allowed
-- Note: Single-element (x) is just a parenthesized expression, not a tuple
pParens :: Parser Expr
pParens = between (symbol "(") (symbol ")") $ do
  firstExpr <- pExpr
  -- Check if there's a comma (indicating tuple with at least 2 elements)
  optional (symbol ",") >>= \case
    Just _ -> do
      -- It's a tuple - parse remaining elements (at least 1 more required)
      restExprs <- sepEndBy pExpr (symbol ",")
      if null restExprs
        then fail "Tuple must have at least 2 elements"
        else return $ ETuple (firstExpr : restExprs)
    Nothing -> return firstExpr  -- Just a parenthesized expression

-- | Self keyword: reference to current process PID
--
-- Returns the process ID of the currently executing process.
-- Used to send messages to oneself or pass PID to others.
--
-- Examples:
--   self              → ESelf
--   other <- {self}   → send self PID to other process
pSelf :: Parser Expr
pSelf = ESelf <$ symbol "self"

-- | Spawn a new process
--
-- Creates a new concurrent process running the specified procedure.
-- Returns a process ID (PID) that can be used to send messages.
--
-- Format: spawn ProcessName(arg1, arg2, ...)
--
-- Examples:
--   spawn Counter(0)           → ESpawn "Counter" [ELiteral 0]
--   spawn Worker(self, data)   → ESpawn "Worker" [ESelf, EVar "data"]
pSpawn :: Parser Expr
pSpawn = ESpawn <$> (symbol "spawn" *> pIdentifier) <*> pArgumentList

-- | Pre-increment and pre-decrement operators
--
-- Pre-increment (++x): increments variable then returns new value
-- Pre-decrement (--x): decrements variable then returns new value
--
-- Examples:
--   ++counter      → EPreInc "counter"
--   --index        → EPreDec "index"
pPreIncDec :: Parser Expr
pPreIncDec = choice
  [ EPreInc <$> (symbol "++" *> pIdentifier),
    EPreDec <$> (symbol "--" *> pIdentifier)
  ]

-- | Post-increment and post-decrement operators
--
-- Post-increment (x++): returns old value then increments variable
-- Post-decrement (x--): returns old value then decrements variable
--
-- Note: This is parsed as a postfix operator on variables
-- Special handling: ++ is also a binary operator (string concatenation)
-- To disambiguate, we use lookahead: if ++ is followed by something that
-- can start an expression (variable, literal, keyword), it's treated as binary
--
-- Examples:
--   counter++      → EPostInc "counter"
--   index--        → EPostDec "index"
--   x ++ y         → (binary concat, not post-increment)
pPostIncDec :: Text -> Parser Expr
pPostIncDec varName = choice
  [ try (EPostInc varName <$ symbol "++" <* notFollowedBy pExprStart),
    try (EPostDec varName <$ symbol "--" <* notFollowedBy pExprStart),
    fail "Not a post-inc/dec"
  ]
  where
    -- Things that can start an expression (checked with notFollowedBy)
    pExprStart = choice
      [ void (symbol "("),
        void (symbol "["),
        void (symbol "{"),
        void (symbol ":"),
        void pLiteral,
        void pIdentifier,
        void (symbol "if"),
        void (symbol "match"),
        void (symbol "receive"),
        void (symbol "spawn"),
        void (symbol "self"),
        void (symbol "scast"),
        void (symbol "rcast"),
        void (symbol "ccast"),
        void (symbol "just"),
        void (symbol "none"),
        void (symbol "ok"),
        void (symbol "ko"),
        void (symbol "-"),
        void (symbol "+"),
        void (symbol "!")
      ]

-- | Variable reference or function call with post-inc/dec support
--
-- Disambiguates between:
--   - Variable reference: identifier without parentheses
--   - Function call: identifier followed by argument list
--   - Post-increment/decrement: identifier followed by ++ or --
--
-- Examples:
--   x              → EVar "x"
--   foo()          → ECall "foo" []
--   add(1, 2)      → ECall "add" [ELiteral 1, ELiteral 2]
--   process(self)  → ECall "process" [ESelf]
--   counter++      → EPostInc "counter"
--   index--        → EPostDec "index"
pVarOrCall :: Parser Expr
pVarOrCall = do
  name <- pIdentifier
  choice
    [ try (pPostIncDec name),
      ECall name <$> pArgumentList,
      pure (EVar name)
    ]

-- | Parse array literal: [expr1, expr2, ...]
--
-- Array literals use square brackets and can contain:
--   - Multiple expressions: [1, 2, 3]
--   - Empty array: []
--
-- Examples:
--   []              → EArray []
--   [1, 2, 3]       → EArray [ELiteral 1, ELiteral 2, ELiteral 3]
--   [x, y, z]       → EArray [EVar "x", EVar "y", EVar "z"]
pArrayOrIndex :: Parser Expr
pArrayOrIndex = between (symbol "[") (symbol "]") $
  EArray <$> sepEndBy pExpr (symbol ",")


-- =============================================================================
-- RECEIVE EXPRESSION (Pattern Matching for Messages)
-- =============================================================================

-- | Receive expression: wait for and match messages
--
-- The receive construct is the core message-handling mechanism in Ratatouille.
-- It blocks the current process until a message arrives that matches one of
-- the provided patterns, then executes the corresponding expression.
--
-- Format:
--   receive {
--     | pattern1 -> expr1
--     | pattern2 -> expr2
--     ...
--   }
--
-- Pattern matching order:
--   - Patterns are tried in order from top to bottom
--   - The first matching pattern is executed
--   - Non-matching messages remain in the mailbox
--
-- Examples:
--   receive {
--     | :increment -> state = state + 1
--     | :decrement -> state = state - 1
--   }
--
--   receive {
--     | {:get, sender} -> sender <- state
--     | {:set, value} -> state = value
--     | _ -> none
--   }
--
-- Note: Pattern parsers are duplicated locally to avoid circular import
-- dependencies with the Pattern module.
pReceive :: Parser Expr
pReceive = symbol "receive" *> braces (EReceive <$> many pReceiveCaseLocal)
  where
    braces = between (symbol "{") (symbol "}")
    
    -- | Parse a single receive case: | pattern -> expr
    --
    -- Examples:
    --   | :ok -> 1
    --   | {x, y} -> x + y
    --   | sender -> sender <- :ack
    pReceiveCaseLocal = Case
      <$> (symbol "|" *> pPatternLocal)
      <*> (symbol "->" *> pExpr)

    -- | Local pattern parser (duplicated to avoid circular import)
    --
    -- Supports:
    --   - Literals: 42, "text"
    --   - Atoms: :ok, :error
    --   - Variables: x, sender
    --   - Wildcards: _
    --   - Tuples: (x, y), (:get, sender) - at least 2 elements
    --   - Varargs: items...
    -- Note: Empty tuples () are not allowed
    -- Note: Single-element (x) is just a parenthesized pattern, not a tuple
    pPatternLocal = choice
      [ try pVarargsLocal,
        try pTuplePatternLocal,
        try (PLiteral <$> pLiteral),
        try (pAtom >>= toAtomPattern),
        PWildcard <$ symbol "_",
        PVar <$> pIdentifier
      ]
    
    -- | Parse tuple pattern (requires at least 2 elements)
    pTuplePatternLocal = between (symbol "(") (symbol ")") $ do
      firstPat <- pPatternLocal
      optional (symbol ",") >>= \case
        Just _ -> do
          restPats <- sepEndBy pPatternLocal (symbol ",")
          if null restPats
            then fail "Tuple pattern must have at least 2 elements"
            else return $ PTuple (firstPat : restPats)
        Nothing -> return firstPat  -- Just a parenthesized pattern
    
    -- | Convert an atom expression to an atom pattern
    toAtomPattern :: Expr -> Parser Pattern
    toAtomPattern (EAtom a) = pure (PAtom a)
    toAtomPattern _ = fail "Expected atom in pattern"

    -- | Variadic pattern: captures remaining elements
    --
    -- Used in tuple patterns to capture all remaining elements.
    --
    -- Examples:
    --   items...
    --   rest...
    pVarargsLocal = PVarargs <$> pIdentifier <* symbol "..."

-- | Match expression: pattern matching on a value
--
-- Similar to receive but matches on an explicit expression instead of
-- waiting for a message.
--
-- Format: match expr { | pattern -> result_expr }
--
-- Examples:
--   match value {
--     | :ok -> "success"
--     | :error -> "failure"
--   }
--
--   match {x, y} {
--     | {0, 0} -> "origin"
--     | {a, b} -> "point"
--   }
pMatch :: Parser Expr
pMatch = do
  _ <- symbol "match"
  matchExpr' <- pExpr
  _ <- symbol "{"
  cases <- many pMatchCaseLocal
  _ <- symbol "}"
  return $ EMatch matchExpr' cases
  where
    -- | Parse a single match case: | pattern -> expr
    pMatchCaseLocal = MatchCase
      <$> (symbol "|" *> pPatternLocal)
      <*> (symbol "->" *> pExpr)
    
    -- Reuse the same pattern parsers from pReceive
    pPatternLocal = choice
      [ try pArrayPatternLocal,
        try pTuplePatternLocal,
        try (PLiteral <$> pLiteral),
        try (pAtom >>= toAtomPattern),
        PWildcard <$ symbol "_",
        try pVarargsLocal,
        PVar <$> pIdentifier
      ]
    
    pArrayPatternLocal = PArray <$> between (symbol "[") (symbol "]")
      (sepEndBy pPatternLocal (symbol ","))
    
    pTuplePatternLocal = between (symbol "(") (symbol ")") $ do
      firstPat <- pPatternLocal
      optional (symbol ",") >>= \case
        Just _ -> do
          restPats <- sepEndBy pPatternLocal (symbol ",")
          if null restPats
            then fail "Tuple pattern must have at least 2 elements"
            else return $ PTuple (firstPat : restPats)
        Nothing -> return firstPat
    
    toAtomPattern :: Expr -> Parser Pattern
    toAtomPattern (EAtom a) = pure (PAtom a)
    toAtomPattern _ = fail "Expected atom in pattern"
    
    pVarargsLocal = PVarargs <$> pIdentifier <* symbol "..."


-- =============================================================================
-- STATEMENT PARSERS (Alphabetically Sorted)
-- =============================================================================
-- Statements are used in blocks and as top-level constructs.
-- All parsers in this section are alphabetically sorted.

-- | Assignment statement: update an existing variable
--
-- Updates the value of an already-defined variable.
-- Will cause a runtime error if the variable doesn't exist.
--
-- Format: varname = expr
--
-- Examples:
--   x = x + 1              → SAssign "x" (x + 1)
--   state = 0              → SAssign "state" 0
--   result = compute(x)    → SAssign "result" (compute(x))
--
-- Note: Uses `try` to distinguish from let statements and expressions
pAssign :: Parser Stmt
pAssign = try $ SAssign <$> pIdentifier <* symbol "=" <*> pExpr

-- | Let statement: introduce a new variable binding
--
-- Creates a new variable in the current scope with an optional type annotation.
-- The variable shadows any existing variable with the same name.
-- Can also parse const bindings using "let const" syntax.
--
-- Format: let varname = expr
--         let varname<type> = expr
--         let const varname = expr
--         let const varname<type> = expr
--
-- Type Annotation:
--   - No annotation (let x = 42)        → Type is automatically deduced
--   - <auto> (let x<auto> = 42)         → Explicit type deduction request
--   - <type> (let x<i32> = 42)          → Explicit type constraint
--
-- Supported Types:
--   Signed integers:   i8, i16, i32, i64
--   Unsigned integers: u8, u16, u32, u64
--   Floating-point:    f32, f64
--   Other types:       string, pid, atom, none, any
--   Tuple types:       (i32, i32), (string, f64), etc. (at least 2 elements)
--
-- Examples:
--   let x = 42                    → SLet "x" Nothing 42 (type deduced)
--   let x<auto> = 42              → SLet "x" (Just TAny) 42 (explicit auto)
--   let x<i32> = 42               → SLet "x" (Just (TNumeric I32)) 42
--   let count<u64> = 0            → SLet "count" (Just (TNumeric U64)) 0
--   let pi<f64> = 3.14159         → SLet "pi" (Just (TNumeric F64)) 3.14159
--   let const PI<i32> = 314       → SConst "PI" (Just (TNumeric I32)) 314
--   let const MAX = 100           → SConst "MAX" Nothing 100
pLet :: Parser Stmt
pLet = do
  _ <- symbol "let"
  -- Check if this is a const binding
  isConst <- optional (symbol "const")
  varName <- pIdentifier
  -- Optional type annotation: <type>
  maybeType <- optional (between (symbol "<") (symbol ">") pType)
  _ <- symbol "="
  value <- pExpr
  case isConst of
    Just _ -> return $ SConst varName maybeType value
    Nothing -> return $ SLet varName maybeType value

-- | Parse destructuring let statement
--
-- Supports destructuring for tuples and arrays with optional type annotations
-- and const qualifiers.
--
-- Syntax:
--   let [pattern] = expr
--   let (pattern) = expr  (alternative tuple syntax)
--
-- Patterns can include:
--   - Simple variables: [x, y, z]
--   - Typed variables: [x<i32>, y<f64>]
--   - Const variables: [const x, y]
--   - Const typed: [const x<i32>, y<f64>]
--   - Wildcards: [x, _, z]
--
-- Examples:
--   let [x, y] = getPoint()              → SLetPattern (PArray [PVar "x", PVar "y"]) ...
--   let [const x<i32>, y<i32>] = pair()  → SLetPattern (PArray [PVarTyped "x" (Just I32) True, ...]) ...
--   let (a, b, c) = getTuple()           → SLetPattern (PTuple [PVar "a", PVar "b", PVar "c"]) ...
--   let [first, _, third] = arr          → SLetPattern (PArray [PVar "first", PWildcard, PVar "third"]) ...
pLetDestructure :: Parser Stmt
pLetDestructure = do
  _ <- symbol "let"
  pattern <- choice
    [ try $ PArray <$> between (symbol "[") (symbol "]") (sepBy pDestructurePattern (symbol ",")),
      PTuple <$> between (symbol "(") (symbol ")") (sepBy pDestructurePattern (symbol ","))
    ]
  _ <- symbol "="
  value <- pExpr
  return $ SLetPattern pattern value
  where
    -- Parse a pattern element in destructuring context
    pDestructurePattern :: Parser Pattern
    pDestructurePattern = choice
      [ PWildcard <$ symbol "_",
        try pTypedVarPattern,
        PVar <$> pIdentifier
      ]
    
    -- Parse typed variable pattern with optional const
    pTypedVarPattern :: Parser Pattern
    pTypedVarPattern = do
      maybeConst <- optional (symbol "const")
      let isConst = case maybeConst of
                      Just _ -> True
                      Nothing -> False
      varName <- pIdentifier
      maybeType <- optional (between (symbol "<") (symbol ">") pType)
      return $ PVarTyped varName maybeType isConst


-- =============================================================================
-- HELPER FUNCTIONS (Alphabetically Sorted)
-- =============================================================================
-- Internal helper functions used by the parsers above.
-- Not exported from the module.

-- | Generic left-associative infix operator parser
--
-- Implements left-associative operator parsing using a fold.
-- This is the core mechanism for handling binary operators.
--
-- Parses: term op term op term
-- As:     ((term op term) op term)
--
-- Used by: pExprMultiplicative, pExprAdditive, pExprComparison,
--          pExprLogicalAnd, pExprLogicalOr
--
-- Examples:
--   1 + 2 + 3        → ((1 + 2) + 3)
--   x && y && z      → ((x && y) && z)
--   a == b == c      → ((a == b) == c)
chainLeft :: Parser Expr -> Parser Op -> Parser Expr
chainLeft termParser opParser = foldl' buildBinOp <$> termParser <*> many ((,) <$> opParser <*> termParser)
  where
    buildBinOp leftExpr (op, rightExpr) = EBinOp op leftExpr rightExpr

-- | Parse the rest of a block after the first statement
--
-- Helper function for pBlock that handles:
--   - Additional statements (let, assignments, sends, or expressions as statements)
--   - Optional final expression (the block's return value)
--
-- The block's value is:
--   - The final expression, if present
--   - 0 (zero) otherwise
--
-- Statement hierarchy in blocks:
--   - let statements (always allowed)
--   - assignments like x = y (allowed as statements)
--   - sends like x <- y (allowed as statements)
--   - Pure expressions (only as final block value)
--
-- Examples:
--   After "let x = 5" might parse:
--     "let y = 10  x = 5  x + y"    → statements + final expr
--     "x = 5  x + 5"                → assignment statement + final expr
--     "let z = 15"                  → final let (no expr after)
parseRestOfBlock :: [Stmt] -> Parser Expr
parseRestOfBlock firstStmts = do
  -- Parse more statements (let, assignment, sends, or expression statements)
  -- We need to parse zero or more statements, then optionally a final expression
  -- Note: We include expression statements here (plain calls, operations, etc.)
  restStmts <- many (try pLet <|> try pAssign <|> try pSendStmt <|> pExprStmt)
  let allStmts = firstStmts <> restStmts
  -- Parse optional final expression (if missing, try to extract from last statement if it's SExpr)
  -- The final expression is a pure value, not an assignment or send
  maybeFinalExpr <- optional (try pExprNoAssignSend)
  let (stmtsToKeep, finalExpr) = case maybeFinalExpr of
        Just expr ->
          -- We found an explicit final expression, keep all statements
          (allStmts, expr)
        Nothing ->
          -- Try to extract expression from last statement if it's SExpr
          case reverse allStmts of
            (SExpr expr) : prevStmts ->
              -- Last statement is just an expression, use it as the final value
              -- Remove the SExpr from statements to avoid double execution
              (reverse prevStmts, expr)
            _ ->
              -- No final expression found, keep all statements and default to 0
              (allStmts, ELiteral (LInt 0))
  pure $ EBlock stmtsToKeep finalExpr
  where
    -- Helper: Parse an expression statement (plain expression without assignment or send)
    pExprStmt :: Parser Stmt
    pExprStmt = SExpr <$> pExprLogicalOr

    -- Helper: Parse a send as a statement
    pSendStmt :: Parser Stmt
    pSendStmt = do
      receiver <- pExprLogicalOr
      _ <- symbol "<-"
      message <- pExprAssign
      pure $ SExpr (ESend receiver message)

    pExprNoAssignSend :: Parser Expr
    pExprNoAssignSend = pExprSend'
      where
        pExprSend' = pExprLogicalOr
