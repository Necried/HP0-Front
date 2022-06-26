{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module TestIndent where

import Control.Monad
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

scn :: Parser ()
scn = L.space space1 empty empty

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

pItem :: Parser String
pItem = lexeme (some (alphaNumChar <|> char '-')) <?> "list item"

pComplexItem :: Parser (String, [String])
pComplexItem = L.indentBlock scn p
  where
    p = do
      header <- pItem
      return (L.IndentMany Nothing (return . (header,)) pLineFold)

pLineFold :: Parser String
pLineFold = L.lineFold scn $ \sc' ->
  let ps = some (alphaNumChar <|> char '-') `sepBy1` try sc'
   in unwords <$> ps <* scn -- (1)

testFromFile :: IO ()
testFromFile = do
  contents <- readFile "test1.p"
  parseTest (pComplexItem <* eof) contents
