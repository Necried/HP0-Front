{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module        : Syntax
-- Description   : This is where the syntax for HP0 is declared.
-- Copyright     : (c) Lucas Dutton, 2020
-- License       : GPL-3
-- Maintainer    : duttonl@mcmaster.ca
-- Stability     : experimental
-- Portability   : POSIX
--
-- The syntax of HP0 is declared here as datatypes to create the abstract
-- syntax tree.
module Syntax where

import Data.List
import Data.Text (Text)

-- | A program is a list of top level declarations, followed by the main program identified by a name
data Program = Program [Declaration] Var [Declaration] [Statement]
  deriving (Show, Eq)

-- | Statements are either:
--
--     * An assignment statement @x := e@
--     * A procedure call: @p(e_1, e_2 ...)@
--     * Compound statement @begin S_1 ; S_2 ; ... ; end@
--     * If-statements. Note that the predicate is @BExpr@, a boolean expression:
--
--         > if c then S
--         > if c then S else T
--
--     * While-statements: @while c do S@. Note the @c@ is also a @BExpr@
data Statement
  = Assign Designator Expr
  | ProcCall (Maybe Designator) Var [Expr]
  | Compose [Statement]
  | If Expr [Statement] (Maybe [Statement])
  | While Expr [Statement]
  deriving (Eq, Show)

data Declaration
  = ConstDecl Var Expr
  | TyDecl Var Type
  | VarDecls [([Var], Type)]
  | ProcDecl Var Params Retty [Declaration] [Statement] -- Or should this just be a singular statement with compose?
  deriving (Eq, Show)

type Retty = Maybe (Var, Type)

type Params = [([Var], Type)]

type VarList = [Var]

-- | Types
data Type
  = Integer
  | Boolean
  | String
  | Array LowerBound UpperBound Type
  | Func [Type] (Maybe Type) -- TODO should be Maybe Type
  | --  | Record [VarIdent]
    Typedef Var
  deriving (Eq, Show)

type LowerBound = Int

type UpperBound = Int

-- | Expressions
data Expr
  = Select Var Designator -- x.f
  | BoolConst Bool
  | V Designator
  | Not Expr
  | BBinary BBinOp Expr Expr
  | RelBinary RBinOp Expr Expr
  | IntConst Int
  | Negate Expr
  | ABinary ABinOp Expr Expr
  | StringConst String
  deriving (Eq, Show)

data Designator = Designator Var (Maybe [Expr])
  deriving (Eq, Show)

data BBinOp
  = And
  | Or
  deriving (Eq, Show)

data RBinOp
  = Equal
  | NEqual
  | LE
  | LTE
  | GT
  | GTE
  deriving (Eq, Show)

data ABinOp
  = Times
  | Div
  | Mod
  | Plus
  | Minus
  deriving (Eq, Show)

type Var = Text -- or string?
