{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module        : PrettyPrint
-- Description   : Convert P0 AST to text form
-- Copyright     : (c) Lucas Dutton, 2020
-- License       : GPL-3
-- Maintainer    : duttonl@mcmaster.ca
-- Stability     : experimental
-- Portability   : POSIX
--
-- Using a haskell pretty printer library, we generate
-- the text-readable program of a P0 program using certain constructs
module PrettyPrint where

import Data.List
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Syntax
import Prelude hiding (GT)

-- | Instance declaration for ops
instance Pretty ABinOp where
  pretty Times = "×"
  pretty Div = "div"
  pretty Mod = "mod"
  pretty Plus = "+"
  pretty Minus = "-"

instance Pretty RBinOp where
  pretty Equal = "="
  pretty NEqual = "≠"
  pretty LE = "<"
  pretty LTE = "≤"
  pretty GT = ">"
  pretty GTE = "≥"

instance Pretty BBinOp where
  pretty And = "and"
  pretty Or = "or"

-- | Instance declaration for designators
instance Pretty Designator where
  pretty (Designator var (Just idx)) = pretty var <> brackets (pretty idx)
  pretty (Designator var Nothing) = pretty var

-- | Instance declaration for expressions
instance Pretty Expr where
  pretty (Select var d) = pretty var <> dot <> pretty d
  pretty (BoolConst b) = pretty b
  pretty (V d) = pretty d
  pretty (Not expr) = "¬" <+> pretty expr
  pretty (Negate expr) = "-" <+> pretty expr
  pretty (BBinary op e1 e2) = pretty e1 <+> pretty op <+> pretty e2
  pretty (RelBinary op e1 e2) = pretty e1 <+> pretty op <+> pretty e2
  pretty (ABinary op e1 e2) = pretty e1 <+> pretty op <+> pretty e2
  pretty (IntConst i) = pretty i

-- | Instance declaration for types
instance Pretty Type where
  pretty Integer = "integer"
  pretty Boolean = "boolean"
  pretty (Array lb ub ty) = pretty lb <+> ".." <+> pretty ub <+> "→" <+> pretty ty

-- | Instance declaration for statements
instance Pretty Statement where
  pretty (Assign d e) = pretty d <+> ":=" <+> pretty e
  pretty (ProcCall mDesig v exprs) = case mDesig of
    Just d -> pretty d <+> "←" <+> pretty v <> parens (sepList exprs ", ")
    Nothing -> pretty v <> parens (sepList exprs ", ")
  pretty (Compose stmts) = sepList stmts "; "
  pretty (If cond th mel) =
    ( nest 4 $ vsep $
        ["if" <+> pretty cond <+> "then"] ++ map pretty th
    )
      <> line
      <> case mel of
        Just el -> nest 4 $ vsep $ ["else"] ++ map pretty el
        Nothing -> ""
  pretty (While cond stmts) =
    nest 4 $ vsep $ ["while" <+> pretty cond <+> "do"] ++ map pretty stmts

instance Pretty Declaration where
  pretty (ConstDecl v e) = "const" <+> pretty v <+> "=" <+> pretty e
  pretty (TyDecl v t) = "type" <+> pretty v <+> "=" <+> pretty t
  pretty (VarDecls vs) =
    let vDec (vars, ty) = sepList vars ", " <> ":" <+> pretty ty
     in hsep $ map (("var " <>) . vDec) vs
  pretty (ProcDecl name params ret declrs stmts) =
    nest 4 $ vsep $
      [ "procedure"
          <+> pretty name
          <> parens (pretty params)
          <+> ( case ret of
                  Just ret -> "→" <+> parens (pretty ret)
                  Nothing -> ""
              )
      ]
        ++ map pretty declrs
        ++ map pretty stmts

instance Pretty Program where
  pretty (Program declrs name decs stmt) =
    let progDeclrs = nest 4 $ vsep $ ["program" <+> pretty name] ++ map pretty decs ++ map pretty stmt
     in vsep (map pretty declrs ++ [progDeclrs])

sepList xs sep = concatWith (\x y -> x <> sep <> y) $ map pretty xs
