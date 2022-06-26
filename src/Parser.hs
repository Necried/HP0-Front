{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module        : Parser
-- Description   : This is where the parser for HP0 is declared.
-- Copyright     : (c) Lucas Dutton, 2020
-- License       : GPL-3
-- Maintainer    : duttonl@mcmaster.ca
-- Stability     : experimental
-- Portability   : POSIX
--
-- We declare the parser for HP0 here.
module Parser where

import Control.Monad
import Control.Monad.Combinators.Expr
import Data.Maybe
import Data.Either
import Data.Text (Text)
import Data.Text.IO as DT (readFile) -- for reading files
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug

import Prelude hiding (GT)

import Lexer
import Syntax

-- | Parse a designator, with the following grammar:
--   > designator ::= ident { "[" expression "]" }
pDesignator :: Parser Designator
pDesignator = lexeme (Designator <$> ident <*> (pIdx <|> pIdent))
  where
    pIndexing = between "[" "]" pExpression
    pIdx = Just <$> some pIndexing
    pIdent = pure Nothing

-- | In the original parser there were four separate parsing procedures for
--   `factor`, `term`, `expression` and `simpleExpression`. Here we use `makeExprParser`
--   from Control.Monad.Combinators.Expr to take care of the operator precedences
--   for us, combining all 4 of the above parsing procedures into a single parser.
pExpression :: Parser Expr
pExpression = makeExprParser pTerm operators

pTerm :: Parser Expr
pTerm =
  parens pExpression
    <|> (BoolConst True) <$ rword "true"
    <|> (BoolConst False) <$ rword "false"
    <|> StringConst <$> stringLiteral
    <|> IntConst <$> number
    <|> V <$> pDesignator

operators :: [[Operator Parser Expr]]
operators =
  [ [ prefix "-" Negate,
      prefix "¬" Not
    ],
    [ binary "≤" (RelBinary LTE),
      binary "<" (RelBinary LE),
      binary "≥" (RelBinary GTE),
      binary ">" (RelBinary GT),
      binary "=" (RelBinary Equal),
      binary "≠" (RelBinary NEqual)
    ],
    [ binary "×" (ABinary Times),
      binary "div" (ABinary Div),
      binary "mod" (ABinary Mod),
      binary "and" (BBinary And)
    ],
    [ binary "+" (ABinary Plus),
      binary "-" (ABinary Minus),
      binary "or" (BBinary Or)
    ]
  ]

binary name f = InfixL (f <$ symbol name)

binaryN name f = InfixN (f <$ symbol name)

prefix name f = Prefix (f <$ symbol name)

-- | pStatementList parsers
--      @ statementList ::= statement {";" statement}
pStatementList :: Parser Statement
pStatementList = dbg "pStatementList" $ do
  stmts <- pStatement `sepBy1` symbol ";"
  case stmts of
    [stmt] -> return stmt
    stmts -> return (Compose stmts)

-- | pStatementBlock parsers
--      @ statementBlock ::= statementList {statementList}
--   Each statement list has to start on a new line.
pStatementBlock :: Parser [Statement]
pStatementBlock = many $ lexemeNewline pStatement

-- | pStatement parses:
--    @ designator ":=" expression |
--    @ designator "←" ident "(" [expression {"," expression}] ")" |
--    @ "if" expression "then" statementSuite ["else" statementSuite] |
--    @ "while" expression "do" statementSuite
pStatement :: Parser Statement
pStatement = dbg "pStatement" $ try pIfThenElse <|> try pWhile <|> try pAssignment <|> pProcCall

-- |
-- In the original parser a statement suite is defined as:
--     @ pStatementSuite :: Parser State -> Parser [Statement]
--     @ pStatementSuite kword = pStatementList <|> pIndentedBlock
-- with the intention of defining parsers with indented blocks.
-- Using megaparsec we split off code blocks that require indentation
pWhile :: Parser Statement
pWhile = L.indentBlock scn p
  where
    p = do
      void (rword "while")
      expr <- dbg "bExpr" pExpression
      void (rword "do")
      return (L.IndentSome Nothing (return . While expr) pStatementList)

pIfThenElse :: Parser Statement
pIfThenElse = dbg "pIfThenElse" $ pIfThen >>= pElse

pIfThen :: Parser (Expr, [Statement])
pIfThen = L.indentBlock scn p
  where
    p = do
      void (rword "if")
      expr <- pExpression
      void (rword "then")
      return (L.IndentSome Nothing (\stmtBlock -> return (expr, stmtBlock)) pStatementList)

pElse :: (Expr, [Statement]) -> Parser Statement
pElse (cond, th) = dbg "pElse" $ L.indentBlock scn (try p <|> return (L.IndentNone (If cond th Nothing)))
  where
    p = do
      void (rword "else")
      return (L.IndentSome Nothing (return . If cond th . Just) pStatementList)

-- | pProcCall parses a procedure call
pProcCall :: Parser Statement
pProcCall = dbg "procCall" $ do
  desig <- try (Just <$> pDesignator <* symbol "←") <|> return Nothing
  var <- ident
  exprList <- parens (pExpression `sepBy` symbol ",")
  return $ ProcCall desig var exprList

pAssignment :: Parser Statement
pAssignment = do
  v <- pDesignator
  void (symbol ":=")
  e <- pExpression
  return (Assign v e)

-- | pTyp parses:
--    @ type ::= ident | expression ".." expression "→" type
pTyp :: Parser Type
pTyp = pBaseTyps <|> (Typedef <$> ident) <|> pArrRange
  where
    pIntTyp = Integer <$ rword "integer"
    pBoolTyp = Boolean <$ rword "boolean"
    pStrTyp = String <$ rword "string"
    pBaseTyps = foldl1 (<|>) [pIntTyp, pBoolTyp, pStrTyp]
    pArrRange = do
      lowerRange <- number
      void (symbol "..")
      upperRange <- number
      void (symbol "→")
      typ <- pTyp
      return (Array lowerRange upperRange typ)

pTypOrFTyp :: Parser Type
pTypOrFTyp = pFTyp <|> pTyp
  where
    pFTyp = Func <$> parens (pTyp `sepBy1` symbol ",")
                 <*> optional (symbol "→" *> pTyp)

-- | pTypeIds parses:
--     @ typedIds ::= ident ":" type {"," ident ":" type}
pTypeIds :: Parser [([Var], Type)]
pTypeIds = p `sepBy` symbol ","
  where
    p = do
      var <- ident `sepBy` symbol ","
      void (symbol ":")
      typ <- pTypOrFTyp
      return (var, typ)

-- | declarations parses:
--     @ declarations ::=
--     @     {"const" ident "=" expression}
--     @     {"type" ident "=" type}
--     @     {"var" typedIds}
--     @     {"procedure" ident "(" [typedIds] ")" [ "→" "(" typedIds ")" ] body}
declarations :: Parser Declaration
declarations =
  dbg "declarations" (consts <|> tys <|> (VarDecls <$> vars) <|> pProcDecl)
  where
    consts = ConstDecl <$> (rword "const" *> ident) <*> (symbol "=" *> pExpression)
    tys = TyDecl <$> (rword "type" *> ident) <*> (symbol "=" *> pTypOrFTyp)
    vars = rword "var" *> pTypeIds

-- Declaration and statements are combined as L.indentBlock does not work
-- if these two declarations were seperated
pProcDecl :: Parser Declaration
pProcDecl = L.nonIndented scn (L.indentBlock scn p)
  where
    p = do
      void (rword "procedure")
      procName <- ident
      params <- parens pTypeIds
      retty <- try (symbol "→" *> (Just <$> parens ((,) <$> ident <*> (symbol ":" *> pTyp)))) <|> return Nothing
      return
        ( L.IndentMany
            (Just $ mkPos 5)
            (\es ->
               let (declr, stmts) = partitionEithers es
               in return (ProcDecl procName params retty declr stmts))
            pStmtOrDeclrs
        )
    pStmtOrDeclrs =
      Left <$> declarations
        <|> Right <$> pStatementList

-- | program parses
--    @ program ::= declarations "program" ident body
program :: Parser Program
program = do
  topLevels <- many (try $ lexemeNewline declarations)
  (progName, declrs, stmts) <- dbg "programDeclr" programDeclr
  pure $ Program topLevels progName declrs stmts

{-
  declrs <- many (lexemeNewline declarations)
  (progName, progDeclrs, progStmts) <- programDeclr
  return (Program declrs progName progDeclrs progStmts)
-}

programDeclr :: Parser (Var, [Declaration], [Statement])
programDeclr = L.nonIndented scn (L.indentBlock scn p)
  where
    p = do
      void (rword "program")
      name <- ident
      return
        ( L.IndentMany
            (Just $ mkPos 5)
            (\es ->
               let (declr, stmts) = partitionEithers es
               in return (name, declr, stmts))
            pStmtOrDeclrs
        )
    pStmtOrDeclrs =
      Left <$> declarations
        <|> Right <$> pStatementList
