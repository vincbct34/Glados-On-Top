{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Common lexing and basic literal/identifier parsers used across parser modules
-}

module Ratatouille.Parser.Common
  ( Parser,
    sc,
    symbol,
    lexeme,
    pIdentifier,
    pIntLiteral,
    pStringLiteral,
    pLiteral,
    pAtom,
    isIdentifierChar,
    reservedWords,
  )
where

import Data.Char (isAlpha, isAlphaNum)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Void (Void)
import Ratatouille.AST (Expr (EAtom), Literal (..))
import Text.Megaparsec
  ( MonadParsec (notFollowedBy, takeWhile1P, takeWhileP, try),
    Parsec,
    empty,
    manyTill,
    satisfy,
    (<|>),
  )
import Text.Megaparsec.Char (char, space1)
import qualified Text.Megaparsec.Char.Lexer as L

-- Type alias for Megaparsec parser used across modules
type Parser = Parsec Void Text

-- Skip whitespace and comments
sc :: Parser ()
sc = L.space space1 empty empty

-- Symbol with trailing space
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- Lexeme helper
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Identifier character predicates
isIdentifierChar :: Char -> Bool
isIdentifierChar c = isAlphaNum c || c == '_'

isIdentifierStartChar :: Char -> Bool
isIdentifierStartChar c = isAlpha c || c == '_'

-- Reserved words that cannot be used as identifiers
reservedWords :: [Text]
reservedWords =
  [ pack "proc", pack "receive", pack "spawn"
  , pack "let", pack "if", pack "then", pack "else"
  , pack "self", pack "none"
  ]
-- Note: "state" is not reserved because it's a special variable that can be used in expressions

-- Parse an identifier as Text
pIdentifier :: Parser Text
pIdentifier = lexeme $ do
  start <- takeWhile1P (Just "identifier start") isIdentifierStartChar
  rest <- takeWhileP (Just "identifier character") isIdentifierChar
  let ident = start <> rest
  if ident `elem` reservedWords
    then fail $ "keyword " ++ T.unpack ident ++ " cannot be used as identifier"
    else return ident

-- Integer literal
pIntLiteral :: Parser Literal
pIntLiteral = lexeme $ do
  num <- L.signed sc L.decimal
  notFollowedBy (satisfy isIdentifierChar)
  return (LInt num)

-- String literal
pStringLiteral :: Parser Literal
pStringLiteral = lexeme $ do
  _ <- char '"'
  s <- manyTill L.charLiteral (char '"')
  return $ LString (pack s)

-- None literal
pNoneLiteral :: Parser Literal
pNoneLiteral = LNone <$ symbol (pack "none")

pLiteral :: Parser Literal
pLiteral = try pNoneLiteral <|> try pIntLiteral <|> pStringLiteral

-- Atom like :ok
pAtom :: Parser Expr
pAtom = EAtom <$> lexeme (char ':' *> takeWhile1P (Just "atom identifier") isIdentifierChar)
