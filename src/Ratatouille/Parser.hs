{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Parser
-}

module Ratatouille.Parser
  ( Parser,
    pProgram,
    pDefinition,
    pStatement,
    sc,
  )
where

import Data.Char (isAlpha, isAlphaNum)
import Data.Functor (void)
import Data.Maybe (fromMaybe, isJust, maybeToList)
import Data.Text (Text, pack)
import Data.Void (Void)
import Ratatouille.AST
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | Type alias for the Megaparsec parser.
type Parser = Parsec Void Text

-- | Skips white space and comments.
sc :: Parser ()
sc = L.space space1 empty empty

-- | Parses a symbol (keyword, operator) and consumes trailing space.
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Takes a parser and makes it consume trailing space after parsing its content.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Predicate to check if a character is valid in an identifier (alphanumeric or underscore).
isIdentifierChar :: Char -> Bool
isIdentifierChar c = isAlphaNum c || c == '_'

-- | Predicate to check if a character is valid as the start of an identifier (alphabetic or underscore).
isIdentifierStartChar :: Char -> Bool
isIdentifierStartChar c = isAlpha c || c == '_'

-- | Parses a generic identifier (variable name, function name, etc.).
pIdentifier :: Parser Text
pIdentifier = lexeme $ do
  start <- takeWhile1P (Just "identifier start") isIdentifierStartChar
  rest <- takeWhileP (Just "identifier character") isIdentifierChar
  return (start <> rest)

-- | Parses an integer literal.
pIntLiteral :: Parser Literal
pIntLiteral = lexeme $ do
  num <- L.signed sc L.decimal
  notFollowedBy (satisfy isIdentifierChar)
  return (LInt num)

-- | Parses a string literal.
pStringLiteral :: Parser Literal
pStringLiteral = lexeme $ do
  _ <- char '"'
  s <- manyTill L.charLiteral (char '"')
  return $ LString (pack s)

-- | Parses any kind of literal (integer or string).
pLiteral :: Parser Literal
pLiteral = pIntLiteral <|> pStringLiteral

-- | Parses an atom (e.g., :myAtom).
pAtom :: Parser Expr
pAtom = EAtom <$> lexeme (char ':' *> takeWhile1P (Just "atom identifier") isIdentifierChar)

-- | Parses a spawn expression: spawn ProcessName(args).
pSpawn :: Parser Expr
pSpawn = do
  _ <- symbol (pack "spawn")
  pName <- pIdentifier
  args <- between (symbol (pack "(")) (symbol (pack ")")) (sepEndBy pExpr (symbol (pack ",")))
  return (ESpawn pName args)

-- | Parses an expression enclosed in parentheses (for operator precedence).
pParens :: Parser Expr
pParens = between (symbol (pack "(")) (symbol (pack ")")) pExpr

-- | Parses content within braces '{}', disambiguating between blocks and tuples.
pBraceContent :: Parser Expr
pBraceContent = between (symbol (pack "{")) (symbol (pack "}")) (try pBlockContent <|> pTupleContent)
  where
    -- \| Attempts to parse a block. Succeeds if it starts with 'let' or contains ';'.
    pBlockContent = do
      -- Check if this looks like a block by trying to detect 'let' keyword or ';'
      _ <- lookAhead (void (try (symbol (pack "let"))) <|> void (try (manyTill anySingle (char ';'))))
      -- Parse statements followed by semicolons
      stmtsWithSemi <- many (try (pStatement <* symbol (pack ";")))
      -- Try to parse one more statement without a semicolon OR a final expression
      maybeLastStmt <- optional (try pLet)
      maybeFinalExpr <- if isJust maybeLastStmt then pure Nothing else optional pExpr
      let allStmts = stmtsWithSemi ++ maybeToList maybeLastStmt
      let finalExpr = fromMaybe (ELiteral (LInt 0)) maybeFinalExpr
      return $ EBlock allStmts finalExpr

    -- \| If not a block, it's a tuple.
    pTupleContent = ETuple <$> sepEndBy pExpr (symbol (pack ","))

-- | Parses a variable expression or a function call.
-- If an identifier is followed by '(', it's a function call; otherwise, it's a variable.
pVarOrCall :: Parser Expr
pVarOrCall = do
  name <- pIdentifier
  maybeArgs <- optional (between (symbol (pack "(")) (symbol (pack ")")) (sepEndBy pExpr (symbol (pack ","))))
  case maybeArgs of
    Nothing -> return (EVar name)
    Just args -> return (ECall name args)

-- | Parses a base expression, which are the "atoms" of an expression tree.
pBaseExpr :: Parser Expr
pBaseExpr =
  (ELiteral <$> pLiteral)
    <|> pAtom
    <|> try pSpawn -- Try spawn before pVarOrCall (spawn is a keyword)
    <|> pVarOrCall -- Replaces pVar to handle both variables and function calls
    <|> pBraceContent -- Uses the unified parser for brace content
    <|> pParens

-- | Parses multiplication and division operators.
pOpMulDiv :: Parser Op
pOpMulDiv = (Mul <$ symbol (pack "*")) <|> (Div <$ symbol (pack "/"))

-- | Parses addition and subtraction operators.
pOpAddSub :: Parser Op
pOpAddSub = (Add <$ symbol (pack "+")) <|> (Sub <$ symbol (pack "-"))

-- | Generic function to build a left-associative infix operator parser.
makeInfixL :: Parser Expr -> Parser Op -> Parser Expr
makeInfixL termParser opParser = do
  term <- termParser
  foldl' (\acc (op, nextTerm) -> EBinOp op acc nextTerm) term
    <$> many
      ((,) <$> opParser <*> termParser)

-- | Parses multiplicative expressions (higher precedence).
pExprMultiplicative :: Parser Expr
pExprMultiplicative = makeInfixL pBaseExpr pOpMulDiv

-- | Parses additive expressions (medium precedence).
pExprAdditive :: Parser Expr
pExprAdditive = makeInfixL pExprMultiplicative pOpAddSub

-- | Parses the message send operator (lowest precedence, right-associative implied).
pExprSend :: Parser Expr
pExprSend = do
  left <- pExprAdditive
  maybeOp <- optional (try (ESend <$ symbol (pack "<-")))
  case maybeOp of
    Nothing -> return left
    Just op -> op left <$> pExprSend

-- | Parses assignment expressions (very low precedence, right-associative).
pExprAssign :: Parser Expr
pExprAssign = try pAssignExpr <|> pExprSend
  where
    pAssignExpr = do
      name <- pIdentifier
      _ <- symbol (pack "=")
      EAssign name <$> pExprAssign

-- | The entry point for parsing any expression.
pExpr :: Parser Expr
pExpr = pExprAssign

-- | Parses a 'let' statement.
pLet :: Parser Stmt
pLet = do
  _ <- symbol (pack "let")
  varName <- pIdentifier
  _ <- symbol (pack "=")
  SLet varName <$> pExpr

-- | Parses an assignment statement (variable = expression).
pAssign :: Parser Stmt
pAssign = try $ do
  varName <- pIdentifier
  _ <- symbol (pack "=")
  SAssign varName <$> pExpr

-- | Parses any statement (either a 'let' binding, an assignment, or an expression).
pStatement :: Parser Stmt
pStatement = pLet <|> pAssign <|> (SExpr <$> pExpr)

-- | Parses a pattern used in 'receive' blocks.
pPattern :: Parser Pattern
pPattern =
  (PVar <$> pIdentifier)
    <|> (PLiteral <$> pLiteral)
    <|> (PAtom <$> lexeme (char ':' *> takeWhile1P (Just "atom") isIdentifierChar))
    <|> (PTuple <$> between (symbol (pack "{")) (symbol (pack "}")) (sepEndBy pPattern (symbol (pack ","))))
    <|> (PWildcard <$ symbol (pack "_"))

-- | Parses a single 'receive' case: '| pattern -> expr'.
pReceiveCase :: Parser ReceiveCase
pReceiveCase = do
  _ <- symbol (pack "|")
  pat <- pPattern
  _ <- symbol (pack "->")
  Case pat <$> pExpr

-- | Parses the body of a process: '{ state: ..., receive { ... } }'.
pProcBody :: Parser ProcBody
pProcBody = between (symbol (pack "{")) (symbol (pack "}")) $ do
  maybeState <- optional (try (symbol (pack "state") *> symbol (pack ":") *> pExpr))
  _ <- optional (symbol (pack ",")) -- Optional comma between 'state' and 'receive'
  maybeReceive <- optional (symbol (pack "receive") *> between (symbol (pack "{")) (symbol (pack "}")) (sepEndBy pReceiveCase (symbol (pack ","))))
  return $ ProcBody maybeState (fromMaybe [] maybeReceive)

-- | Parses a full process definition: 'proc Name(params) { body }'.
pProcDef :: Parser ProcDefinition
pProcDef = do
  _ <- symbol (pack "proc")
  name <- pIdentifier
  params <- between (symbol (pack "(")) (symbol (pack ")")) (sepEndBy pIdentifier (symbol (pack ",")))
  ProcDef name params <$> pProcBody

-- | Parses a top-level definition (either a process or a statement).
pDefinition :: Parser Definition
pDefinition = (DProc <$> pProcDef) <|> (DStmt <$> pStatement)

-- | pProgram: The main parser for a complete Ratatouille source file.
-- It parses a sequence of top-level definitions, separated by semicolons.
-- The semicolon is mandatory between definitions, but optional at the very end.
pProgram :: Parser Program
pProgram = Program <$> (sc *> sepEndBy pDefinition (symbol (pack ";")) <* eof)
