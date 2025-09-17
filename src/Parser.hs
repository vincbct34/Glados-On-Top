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
    parseTuple,
    parseTruple,
    withError
) where

import Data.Char (toUpper, digitToInt)
import Control.Applicative (liftA2, liftA3, (<|>), empty, Alternative(..))
import Data.Traversable (sequenceA)

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
    (x:_)           -> Left (UnexpectedChar x Nothing)
    []              -> Left (UnexpectedEOF Nothing)

-- | Attach a custom error to a parser if it fails.
withError :: Parser a -> ParserError -> Parser a
withError p err = Parser $ \input ->
    case runParser p input of
        Left _  -> Left err
        result  -> result

-- | Succeeds for a specific character.
parseChar :: Char -> Parser Char
parseChar c = parseSatisfy (== c) `withError` ExpectedChar c Nothing

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

-- | Parse zero or more occurrences.
parseMany :: Parser a -> Parser [a]
parseMany p = (:) <$> p <*> parseMany p <|> pure []

-- | Parse one or more occurrences.
parseSome :: Parser a -> Parser [a]
parseSome p = (:) <$> p <*> parseMany p

-- | Parse an unsigned integer.
parseUInt :: Parser Int
parseUInt = withError parser (GenericError "Expected unsigned integer")
    where
        parser = do
            ds <- parseSome (parseAnyChar ['0'..'9'])
            return $ foldl (\acc d -> acc * 10 + digitToInt d) 0 ds

-- | Parse a signed integer.
parseInt :: Parser Int
parseInt = (negate <$> (parseChar '-' *> parseUInt)) <|>
           (parseChar '+' *> parseUInt) <|> parseUInt

-- | Parse two values, return the second.
parseRight :: Parser a -> Parser b -> Parser b
parseRight = parseAndWith (flip const)

-- | Parse two values, return the first.
parseLeft :: Parser a -> Parser b -> Parser a
parseLeft = parseAndWith const

-- | Parse a tuple separated by a comma.
parseTuple :: Parser a -> Parser (a, a)
parseTuple p = liftA2 (,) p (parseChar ',' *> p)

-- | Parse a triple of integers separated by commas.
parseTruple :: Parser (Int, Int, Int)
parseTruple = liftA3 (,,)
    (parseInt <* parseChar ',')
    (parseInt <* parseChar ',')
    parseInt

-------------------------------------------------------------------------------
-- | Add context information to an error message.
withContext :: String -> ParserError -> ParserError
withContext context err = case err of
    GenericError msg -> GenericError (message)
    RError (RuntimeError msg loc) -> RError (RuntimeError (message) loc)
    PError (InvalidSyntax msg loc) -> PError (InvalidSyntax (message) loc)
    _ -> err
      where message = context ++ ": " ++ msg

-- | Combine multiple errors into one.
chainErrors :: [ParserError] -> ParserError
chainErrors [] = GenericError "Unknown error"
chainErrors [e] = e
chainErrors errors = GenericError $ "Multiple errors: " ++ unlines (map show errors)
