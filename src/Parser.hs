{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Parser
-}

{-# LANGUAGE LambdaCase #-}

module Parser (
    -- * Types
    Parser(..),
    ParserError(..),
    ParseError(..),
    SemanticError(..),
    RuntimeError(..),
    SourceLocation(..),
    -- * Source Location
    defaultLocation,
    -- * Error Handling
    withContext,
    chainErrors,
    -- * Parser Combinators
    parseSatisfy,
    parseChar,
    parseCharCase,
    parseAnyChar,
    parseAnyCharCase,
    parseAnd,
    parseAndWith,
    parseMany,
    parseSome,
    parseUInt,
    parseInt,
    parseRight,
    parseLeft,
    withError,
    ---- LISP Parsers
    parseLispValue,
    parseAtom,
    parseNumber,
    parseString,
    parseBoolean,
    parseList,
    parseExpression,
    parseWhitespace,
    parseEOF,
    parseComment
) where

import Control.Applicative (Alternative(..), empty, liftA2, many, some, (<|>))
import Data.Char (toUpper, isAlpha, isAlphaNum, isDigit, isSpace)
import Types (LispValue(..))

-------------------------------------------------------------------------------
-- | Represents a location in the source code for error reporting.
data SourceLocation = SourceLocation {
    srcLine :: Int,    -- ^ Line number
    srcColumn :: Int,  -- ^ Column number
    srcOffset :: Int   -- ^ Character offset
} deriving (Show, Eq)

-- | Default source location (start of file).
defaultLocation :: SourceLocation
defaultLocation = SourceLocation 1 1 0

-------------------------------------------------------------------------------
-- | Error type for parsing failures.
data ParseError
    = UnexpectedChar Char (Maybe SourceLocation)
    | UnexpectedEOF (Maybe SourceLocation)
    | ExpectedChar Char (Maybe SourceLocation)
    | ExpectedString String (Maybe SourceLocation)
    | InvalidSyntax String (Maybe SourceLocation)
    deriving (Eq)

-- | Error type for semantic failures.
data SemanticError
    = UndefinedVariable String (Maybe SourceLocation)
    | TypeMismatch String String (Maybe SourceLocation)
    | ArityMismatch String Int Int (Maybe SourceLocation)
    | InvalidOperation String (Maybe SourceLocation)
    | DivisionByZero (Maybe SourceLocation)
    deriving (Eq)

-- | Error type for runtime failures.
data RuntimeError
    = RuntimeError String (Maybe SourceLocation)
    | StackOverflow (Maybe SourceLocation)
    | OutOfMemory (Maybe SourceLocation)
    deriving (Eq)

-- | Unified error type for all parser errors.
data ParserError
    = PError ParseError
    | SError SemanticError
    | RError RuntimeError
    | GenericError String
    deriving (Eq)

-------------------------------------------------------------------------------
-- | Helper to format source location for error messages.
showLoc :: Maybe SourceLocation -> String
showLoc Nothing = ""
showLoc (Just (SourceLocation line col _)) = " at line " ++ show line ++ ", column " ++ show col

instance Show ParseError where
    show (UnexpectedChar c loc) = "Parse error" ++ showLoc loc ++ ": unexpected character '" ++ [c] ++ "'"
    show (UnexpectedEOF loc) = "Parse error" ++ showLoc loc ++ ": unexpected end of input"
    show (ExpectedChar c loc) = "Parse error" ++ showLoc loc ++ ": expected character '" ++ [c] ++ "'"
    show (ExpectedString s loc) = "Parse error" ++ showLoc loc ++ ": expected \"" ++ s ++ "\""
    show (InvalidSyntax msg loc) = "Syntax error" ++ showLoc loc ++ ": " ++ msg

instance Show SemanticError where
    show (UndefinedVariable name loc) = "Reference error" ++ showLoc loc ++ ": undefined variable '" ++ name ++ "'"
    show (TypeMismatch expected actual loc) = "Type error" ++ showLoc loc ++ ": expected " ++ expected ++ ", got " ++ actual
    show (ArityMismatch funcName expected actual loc) = "Arity error" ++ showLoc loc ++ ": function '" ++ funcName ++ "' expects " ++ show expected ++ " arguments, but got " ++ show actual
    show (InvalidOperation msg loc) = "Semantic error" ++ showLoc loc ++ ": " ++ msg
    show (DivisionByZero loc) = "Semantic error" ++ showLoc loc ++ ": division by zero"

instance Show RuntimeError where
    show (RuntimeError msg loc) = "Runtime error" ++ showLoc loc ++ ": " ++ msg
    show (StackOverflow loc) = "Runtime error" ++ showLoc loc ++ ": stack overflow"
    show (OutOfMemory loc) = "Runtime error" ++ showLoc loc ++ ": out of memory"

instance Show ParserError where
    show (PError err) = show err
    show (SError err) = show err
    show (RError err) = show err
    show (GenericError msg) = "Error: " ++ msg

-------------------------------------------------------------------------------
-- | Parser type: wraps a function from input to either an error or a result.
data Parser a = Parser {
    runParser :: String -> Either ParserError (a, String)
}

instance Functor Parser where
    fmap f p = Parser $ \input ->
        fmap (\(a, rest) -> (f a, rest)) (runParser p input)

instance Applicative Parser where
    pure a = Parser (\input -> Right (a, input))
    pf <*> pa = Parser $ \input -> do
        (f, rest)      <- runParser pf input
        (a, remaining) <- runParser pa rest
        return (f a, remaining)

instance Alternative Parser where
    empty = Parser $ const (Left (GenericError "empty"))
    p1 <|> p2 = Parser $ \input ->
        case runParser p1 input of
            Right res -> Right res
            Left _  -> runParser p2 input

instance Monad Parser where
    (>>=) p f = Parser $ \input ->
        case runParser p input of
            Right (a, rest) -> runParser (f a) rest
            Left err        -> Left err

-------------------------------------------------------------------------------
-- | Succeeds for a character satisfying the predicate.
parseSatisfy :: (Char -> Bool) -> Parser Char
parseSatisfy pred = Parser $ \case
    (x:xs) | pred x -> Right (x, xs)
    (x:_)           -> Left (PError (UnexpectedChar x Nothing))
    []              -> Left (PError (UnexpectedEOF Nothing))

-- | Attach a custom error to a parser if it fails.
withError :: Parser a -> ParserError -> Parser a
withError p err = Parser $ \input ->
    case runParser p input of
        Left _  -> Left err
        result  -> result

-- | Succeeds for a specific character.
parseChar :: Char -> Parser Char
parseChar c = parseSatisfy (== c) `withError` PError (ExpectedChar c Nothing)

-- | Succeeds for a character, case-insensitive.
parseCharCase :: Char -> Parser Char
parseCharCase c = parseSatisfy (\x -> toUpper x == toUpper c)

-- | Succeeds for any character in the given string.
parseAnyChar :: String -> Parser Char
parseAnyChar cs = parseSatisfy (`elem` cs)

-- | Succeeds for any character in the given string, case-insensitive.
parseAnyCharCase :: String -> Parser Char
parseAnyCharCase cs = parseSatisfy (\x -> toUpper x `elem` map toUpper cs)

-- | Parse two values and return a tuple.
parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd = liftA2 (,)

-- | Parse two values and combine them with a function.
parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith = liftA2

-- | Parse zero or more occurrences (alias for 'many').
parseMany :: Parser a -> Parser [a]
parseMany = many

-- | Parse one or more occurrences (alias for 'some').
parseSome :: Parser a -> Parser [a]
parseSome = some

-- | Parse an unsigned integer.
parseUInt :: Parser Int
parseUInt = withError parser (GenericError "Expected unsigned integer")
  where
    parser = read <$> parseSome (parseAnyChar ['0'..'9'])

-- | Parse a signed integer.
parseInt :: Parser Int
parseInt = (negate <$> (parseChar '-' *> parseUInt))
        <|> (parseChar '+' *> parseUInt)
        <|> parseUInt

-- | Parse two values, return the second (alias for '*>').
parseRight :: Parser a -> Parser b -> Parser b
parseRight = (*>)

-- | Parse two values, return the first (alias for '<*').
parseLeft :: Parser a -> Parser b -> Parser a
parseLeft = (<*)

-------------------------------------------------------------------------------
-- | Add context information to an error message.
withContext :: String -> ParserError -> ParserError
withContext context err = case err of
    GenericError msg -> GenericError (context ++ ": " ++ msg)
    RError (RuntimeError msg loc) -> RError (RuntimeError (context ++ ": " ++ msg) loc)
    PError (InvalidSyntax msg loc) -> PError (InvalidSyntax (context ++ ": " ++ msg) loc)
    _ -> err

-- | Combine multiple errors into one.
chainErrors :: [ParserError] -> ParserError
chainErrors [] = GenericError "Unknown error"
chainErrors [e] = e
chainErrors errors = GenericError $ "Multiple errors: \n" ++ unlines (map extractMsg errors)
  where
    extractMsg (GenericError msg) = msg
    extractMsg err = show err

-------------------------------------------------------------------------------
-- | LISP-specific parsers

-- | Parse whitespace (spaces, tabs, newlines) and comments
parseWhitespace :: Parser ()
parseWhitespace = () <$ parseMany parseWhitespaceOrComment
  where
    parseWhitespaceOrComment = 
        (() <$ parseSatisfy isSpace) <|>
        (() <$ (parseChar ';' *> parseMany (parseSatisfy (/= '\n')) *> ((() <$ parseChar '\n') <|> parseEOF)))

-- | Parse a single comment line
parseComment :: Parser ()
parseComment = () <$ (parseChar ';' *> parseMany (parseSatisfy (/= '\n')))

-- | Parse end of input
parseEOF :: Parser ()
parseEOF = Parser $ \case
    "" -> Right ((), "")
    (c:_) -> Left (PError (UnexpectedChar c Nothing))

-- | Parse an atom (symbol/identifier)
parseAtom :: Parser LispValue
parseAtom = do
    first <- parseSatisfy (\c -> isAlpha c || c `elem` "+-*/=<>!?_")
    rest <- parseMany (parseSatisfy (\c -> isAlphaNum c || c `elem` "+-*/=<>!?_-"))
    return $ Atom (first : rest)

-- | Parse a number (integer)
parseNumber :: Parser LispValue
parseNumber = Number . toInteger <$> parseInt

-- | Parse a string literal
parseString :: Parser LispValue
parseString = do
    _ <- parseChar '"'
    content <- parseMany (parseSatisfy (/= '"'))
    _ <- parseChar '"' `withError` PError (ExpectedChar '"' Nothing)
    return $ String content

-- | Parse a boolean (#t or #f)
parseBoolean :: Parser LispValue
parseBoolean = 
    (parseChar '#' *> parseChar 't' *> pure (Boolean True)) <|>
    (parseChar '#' *> parseChar 'f' *> pure (Boolean False))

-- | Parse a list (including empty list)
parseList :: Parser LispValue
parseList = do
    _ <- parseChar '('
    parseWhitespace
    elements <- parseMany (parseWhitespace *> parseLispValue <* parseWhitespace)
    parseWhitespace
    _ <- parseChar ')'
    return $ case elements of
        [] -> Nil
        xs -> List xs

-- | Parse any LISP value
parseLispValue :: Parser LispValue
parseLispValue = 
    parseNumber <|>
    parseString <|>
    parseBoolean <|>
    parseList <|>
    parseAtom

-- | Parse a complete LISP expression
parseExpression :: Parser LispValue
parseExpression = parseWhitespace *> parseLispValue <* parseWhitespace <* parseEOF
