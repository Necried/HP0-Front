{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module        : Lexer
-- Description   : This is where the lexer for HP0 is declared.
-- Copyright     : (c) Lucas Dutton, 2020
-- License       : GPL-3
-- Maintainer    : duttonl@mcmaster.ca
-- Stability     : experimental
-- Portability   : POSIX
--
-- We declare the lexer for P0 here.
-- In addition to following closely the original source file (SC.py), we define additional parser primitives
-- and Megaparsec-related helpers.
module Lexer where

import Control.Monad
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- | @sc@ is short for "space consumer".
--   Megaparsec follows the strategy "Assume no white space before token and consume all white space after token"
scn :: Parser ()
scn =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "{" "}")

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t'))  (L.skipLineComment "//") (L.skipBlockComment "{" "}")

-- | @lexeme@ wraps lexemes that picks all trailing white space using the supplied space consumer
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | @lexemeBlock@ is used for lexing whitespace for items inside an indented block
lexemeNewline :: Parser a -> Parser a
lexemeNewline = L.lexeme scn

-- | @symbol@ matches given text using and then picks up trailing whitespace
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | @parens@ takes a parser and returns a parser that parses parenthesis
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | @number@ parses an integer
number :: Parser Int
number = lexeme $ do
  n <- L.decimal
  if n >= 2 ^ 31
    then fail "number too large"
    else return n

-- | @rword@ ONLY parses reserved words or keywords. In P0 the procedure checks if a keyword is parsed
-- and sets the scanner state, but here we separate it out as a standalone parser
rword :: Text -> Parser Text
rword keyword = lexeme (string keyword <* notFollowedBy alphaNumChar)

rwordBlock :: Text -> Parser Text
rwordBlock keyword = lexemeNewline (string keyword <* notFollowedBy alphaNumChar)

keywords :: [Text]
keywords =
  [ "div",
    "mod",
    "and",
    "or",
    "of",
    "then",
    "do",
    "not",
    "end",
    "else",
    "if",
    "while",
    "array",
    "record",
    "const",
    "type",
    "var",
    "procedure",
    "begin",
    "program"
  ]

-- | @ident@ recognizes a standard identifier
ident :: Parser Text
ident = (lexeme . try) (p >>= check)
  where
    p = fmap pack $ (:) <$> letterChar
                        <*> many alphaNumChar
    check x = if x `elem` keywords
      then fail $ "keyword " <> show x <> " cannot be an identifier"
      else return x

-- | @stringLiteral@ parses a literal string
stringLiteral :: Parser String
stringLiteral = char '"' >> manyTill L.charLiteral (char '"')

-- TODO:
-- In python3:
-- x = bytes([0xf0, 0x9f, 0x99, 0x82])
-- s = x.decode()
