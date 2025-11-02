{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Common lexing and basic literal/identifier parsers
-}

{-# LANGUAGE LambdaCase #-}

module Ratatouille.Parser.Common
  ( Parser
  , sc
  , symbol
  , lexeme
  , pIdentifier
  , pIntLiteral
  , pStringLiteral
  , pLiteral
  , pAtom
  , pType
  , pReturnType
  , pNumericType
  , isIdentifierChar
  , reservedWords
  )
where

import Data.Char (isAlpha, isAlphaNum)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Void (Void)
import Ratatouille.AST
  ( Expr (EAtom)
  , Literal (..)
  , NumericType (..)
  , Type (..)
  )
import Text.Megaparsec
  ( MonadParsec (notFollowedBy, takeWhile1P, takeWhileP, try)
  , Parsec
  , between
  , choice
  , manyTill
  , optional
  , satisfy
  , sepBy
  , (<|>)
  )
import Text.Megaparsec.Char (char, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

-- | Type alias for Megaparsec parser used across modules
type Parser = Parsec Void Text

-- | Skip whitespace and comments
sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where
    lineComment = L.skipLineComment (pack "//")
    blockComment = L.skipBlockComment (pack "/*") (pack "*/")

-- | Parse symbol with trailing space
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Lexeme combinator (skips trailing whitespace)
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Check if character is valid in identifier
isIdentifierChar :: Char -> Bool
isIdentifierChar c = isAlphaNum c || c == '_'

-- | Check if character can start an identifier
isIdentifierStartChar :: Char -> Bool
isIdentifierStartChar c = isAlpha c || c == '_'

-- | Reserved keywords that cannot be used as identifiers
reservedWords :: [Text]
reservedWords =
  [ pack "proc", pack "receive", pack "spawn"
  , pack "let", pack "const", pack "if", pack "then", pack "else"
  , pack "self", pack "none", pack "void"
  , pack "true", pack "false"
  , pack "scast", pack "rcast", pack "ccast"
  , pack "just", pack "none", pack "ok", pack "ko"
  , pack "import", pack "from"
  , pack "match"
  ]

-- | Parse an identifier (not a reserved word)
pIdentifier :: Parser Text
pIdentifier = lexeme $ do
  start <- takeWhile1P (Just "identifier start") isIdentifierStartChar
  rest <- takeWhileP (Just "identifier character") isIdentifierChar
  let ident = start <> rest
  if ident `elem` reservedWords
    then fail $ "keyword " ++ T.unpack ident
                  ++ " cannot be used as identifier"
    else return ident

-- | Parse integer literal with optional type suffix
-- Supports: 42, 42i32, 100u8, etc.
pIntLiteral :: Parser Literal
pIntLiteral = lexeme $ do
  num <- L.signed sc L.decimal
  maybeType <- optional (try pNumericTypeSuffix)
  notFollowedBy (satisfy isIdentifierChar)
  return $ case maybeType of
    Nothing -> LInt num
    Just numType -> LTypedInt numType num

-- | Parse float literal with optional type suffix
-- Supports: 3.14, 3.14f32, 2.5f64
pFloatLiteral :: Parser Literal
pFloatLiteral = lexeme $ do
  num <- L.signed sc L.float
  maybeType <- optional (try pNumericTypeSuffix)
  notFollowedBy (satisfy isIdentifierChar)
  return $ case maybeType of
    Nothing -> LFloat num
    Just numType -> LTypedFloat numType num

-- | Parse numeric type suffix
pNumericTypeSuffix :: Parser NumericType
pNumericTypeSuffix =
  choice
    [ I8 <$ string (pack "i8")
    , I16 <$ string (pack "i16")
    , I32 <$ string (pack "i32")
    , I64 <$ string (pack "i64")
    , U8 <$ string (pack "u8")
    , U16 <$ string (pack "u16")
    , U32 <$ string (pack "u32")
    , U64 <$ string (pack "u64")
    , F32 <$ string (pack "f32")
    , F64 <$ string (pack "f64")
    ]

-- | Parse string literal
pStringLiteral :: Parser Literal
pStringLiteral = lexeme $ do
  _ <- char '"'
  s <- manyTill L.charLiteral (char '"')
  return $ LString (pack s)

-- | Parse none literal
pNoneLiteral :: Parser Literal
pNoneLiteral = LNone <$ symbol (pack "none")

-- | Parse boolean literals
pBoolLiteral :: Parser Literal
pBoolLiteral =
  choice
    [ LBool True <$ symbol (pack "true")
    , LBool False <$ symbol (pack "false")
    ]

-- | Parse any literal value
-- Float must be tried before integer to handle decimal points
pLiteral :: Parser Literal
pLiteral =
  choice
    [ try pNoneLiteral
    , try pBoolLiteral
    , try pFloatLiteral
    , try pIntLiteral
    , pStringLiteral
    ]

-- | Parse atom expression like :ok
pAtom :: Parser Expr
pAtom =
  EAtom
    <$> lexeme
          (char ':' *> takeWhile1P (Just "atom identifier") isIdentifierChar)

-- =============================================================================
-- TYPE ANNOTATION PARSERS
-- =============================================================================

-- | Parse a numeric type
pNumericType :: Parser NumericType
pNumericType =
  lexeme $
    choice
      [ I8 <$ string (pack "i8")
      , I16 <$ string (pack "i16")
      , I32 <$ string (pack "i32")
      , I64 <$ string (pack "i64")
      , U8 <$ string (pack "u8")
      , U16 <$ string (pack "u16")
      , U32 <$ string (pack "u32")
      , U64 <$ string (pack "u64")
      , F32 <$ string (pack "f32")
      , F64 <$ string (pack "f64")
      ]

-- | Parse a type annotation (for variable types, NOT return types)
-- Examples:
--   i32        → TNumeric I32
--   string     → TString
--   bool       → TBool
--   {i32, i32} → TTuple [TNumeric I32, TNumeric I32]
--   i32?       → TMaybe (TNumeric I32)
--   i32!string → TEither (TNumeric I32) TString
--   (i32?)?    → TMaybe (TMaybe (TNumeric I32)) -- requires parentheses for nesting
--   (i32!string)? → TMaybe (TEither (TNumeric I32) TString) -- requires parentheses
--   pid        → TPid
--   auto       → TAny (explicit type deduction request)
--   any        → TAny (accepts any type)
-- Note: 'void' is NOT allowed here - it's only for return types
-- Note: Nested Maybe/Either requires parentheses to avoid ambiguity
pType :: Parser Type
pType = pTypeBase >>= pTypePostfix
  where
    -- Parse base type (no Maybe/Either postfix yet)
    pTypeBase = choice
      [ pSimpleType,
        try pArrayType,
        pParenType  -- Parenthesized type for nesting AND tuples
      ]

    -- Parse simple types (keywords and identifiers)
    pSimpleType = choice
      [ TNumeric <$> pNumericType,
        TString <$ symbol (pack "string"),
        TBool <$ symbol (pack "bool"),
        TPid <$ symbol (pack "pid"),
        TAtom <$ symbol (pack "atom"),
        TNone <$ symbol (pack "none"),
        TAny <$ (symbol (pack "auto") <|> symbol (pack "any"))
      ]

    -- Parse postfix operators (? for Maybe, ! for Either)
    pTypePostfix base = choice
      [ lexeme (char '?') >> return (TMaybe base),
        lexeme (char '!') >> TEither base <$> pTypeBase,
        return base
      ]

-- | Parse a parenthesized type (for nesting Maybe/Either)
-- Examples:
--   (i32)      → TNumeric I32
--   (i32?)     → TMaybe (TNumeric I32)
--   (i32?)?    → TMaybe (TMaybe (TNumeric I32))
-- | Parse a parenthesized type or tuple type
-- Examples:
--   (i32)          → TNumeric I32 (single type in parens)
--   (i32, string)  → TTuple [TNumeric I32, TString] (tuple - at least 2 elements)
--   (i32?)         → TMaybe (TNumeric I32) (parenthesized for nesting)
--   (i32?)?        → TMaybe (TMaybe (TNumeric I32))
-- Note: Empty tuples () are not allowed
-- Note: Single-element tuples (x) are just parenthesized expressions/types
pParenType :: Parser Type
pParenType = between (symbol (pack "(")) (symbol (pack ")")) pParenContent
  where
    pParenContent = do
      firstType <- pType
      optional (symbol (pack ",")) >>= \case
        Just _ -> pTupleTypes firstType
        Nothing -> return firstType

    pTupleTypes firstType = do
      restTypes <- sepBy pType (symbol (pack ","))
      if null restTypes
        then fail "Tuple must have at least 2 elements"
        else return $ TTuple (firstType : restTypes)

-- | Parse a return type annotation (allows 'void')
-- Examples:
--   i32        → TNumeric I32
--   void       → TVoid (no value/unit type)
--   {i32, i32} → TTuple [TNumeric I32, TNumeric I32]
-- Used in function signatures like: {:add, a, b} -> i32
pReturnType :: Parser Type
pReturnType = choice
  [ TVoid <$ symbol (pack "void"),
    pType
  ]

-- | Parse an array type: [ElementType] or [ElementType, Size]
-- Examples:
--   [i32]       → TArray (TNumeric I32) Nothing (dynamic vector)
--   [i32, 10]   → TArray (TNumeric I32) (Just 10) (fixed-size array)
--   [string]    → TArray TString Nothing
-- TODO: Support allocator specification [i32, Allocator]
pArrayType :: Parser Type
pArrayType = between (symbol (pack "[")) (symbol (pack "]")) $ do
  elemType <- pType
  maybeSize <- optional (symbol (pack ",") *> L.decimal)
  return $ TArray elemType maybeSize
