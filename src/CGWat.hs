{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Module        : CGwat
-- Description   : Convert P0 AST to WebAssembly text form
-- Copyright     : (c) Lucas Dutton, 2020
-- License       : GPL-3
-- Maintainer    : duttonl@mcmaster.ca
-- Stability     : experimental
-- Portability   : POSIX
--
--
-- Code generation for web assembly format
module CGWat where

{-

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Functor.Identity
import Data.Map ((!), Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Void (Void)
import ST
import Syntax
import Prelude hiding (GT)

-- | Codegen state
data CodegenState
  = CodegenState
      { symtab :: SymTab,
        curlev :: Level,
        memsize :: Integer,
        asm :: [Doc ()]
      }

newtype Codegen a = Codegen {runCodegen :: State CodegenState a}
  deriving (Functor, Applicative, Monad, MonadState CodegenState)

-- | Fetches information of a variable from the symbol table
fetch :: Var -> Codegen (Level, SymEntry)
fetch v = do
  cgState@CodegenState {..} <- get
  case lookup v (symtab ! curlev) of
    Just entry -> return (curlev, entry)
    Nothing -> case lookup v (symtab ! Global) of
      Just entry -> return (Global, entry)
      Nothing -> case lookup v (symtab ! Memory) of
        Just entry -> return (Memory, entry)
        Nothing -> error "not found"

-- | Following functions "generate code" for all P0 types by determining
--     the size of objects
genTy :: Type -> Codegen Integer
genTy Boolean = return 1
genTy Integer = return 4
genTy (Array x y ty) = do
  size <- genTy ty
  return $ (y - x) * size
genTy (Typedef v) = do
  symtab <- gets symtab
  (_, (symTy, _)) <- fetch v
  case symTy of
    Type ty -> genTy ty
    _ -> error "Wrong type"

-- | genGlobalVars allocates a global WebAssembly variable by the same
--    name if type is Int or Bool, or reserves space in memory if it is Array or Record
--   Parameter sc contains the top scope with all declarations parsed so far, only variable
--   declarations from index start on in the top scope are considered

-- | @ genGlobalVars decl@ adds a global variable to the symbol table and generates code
--  for it
genGlobalVars :: Declaration -> Codegen ()
genGlobalVars (VarDecls vdecls) =
  forM_ vdecls $ \(vs, ty) -> do
    case ty of
      Integer -> forM_ vs (genSimple ty)
      Boolean -> forM_ vs (genSimple ty)
      Array lb ub ty -> forM_ vs (genArrType lb ub ty)
  where
    genSimple :: Type -> Var -> Codegen ()
    genSimple ty v = do
      let globDec = parens $ "global $" <> pretty v <+> "(mut i32) i32.const 0"
          newEntry = (v, (Var ty, Nothing))
      modify $ \cgState@CodegenState {..} ->
        cgState {symtab = Map.alter (fmap (newEntry :)) Global symtab, asm = asm ++ [globDec]}
    genArrType :: Integer -> Integer -> Type -> Var -> Codegen ()
    genArrType lb ub ty v = do
      cgState@CodegenState {..} <- get
      sz <- genTy ty
      let newEntry = (v, (Var (Array lb ub ty), Just memsize))
      put (cgState {symtab = Map.alter (fmap (newEntry :)) Memory symtab, memsize = memsize + sz})

-- | @genLocalVars@ generates local declarations. Only works for integer or boolean
--     types as of now
genLocalVars :: Declaration -> Codegen ()
genLocalVars (VarDecls vdecls) =
  forM_ vdecls $ \(vs, ty) -> do
    case ty of
      Integer -> forM_ vs (genSimple ty)
      Boolean -> forM_ vs (genSimple ty)
      _ -> error "not supported"
  where
    genSimple :: Type -> Var -> Codegen ()
    genSimple ty v = do
      let localDec = parens $ "local $" <> pretty v <+> "(mut i32) i32.const 0"
          newEntry = (v, (Var ty, Nothing))
      modify $ \cgState@CodegenState {..} ->
        cgState {symtab = Map.alter (fmap (newEntry :)) curlev symtab, asm = asm ++ [localDec]}

-- | @loadItem x@ generates code for loading item @x@ on the expression stack. @x@ is
--    assumed global Var, local Var, stack Var, memory Var, local Ref, stack Ref, Const.
loadItem :: Designator -> Codegen SymType
loadItem (Designator v Nothing) = do
  (lvl, (symTy, _)) <- fetch v
  let asmEntry = case lvl of
        Global -> "global.get $" <> pretty v
        (Nested _) -> "local.get $" <> pretty v
  modify $ \cgState@CodegenState {..} ->
    cgState {asm = asm ++ [asmEntry]}
  case symTy of
    ty@(Var _) -> return ty
    _ -> error "bad type"
loadItem (Designator v (Just expr)) = do
  (lvl, (symTy, mAdr)) <- fetch v
  case (symTy, mAdr) of
    (Var (Array lb ub ty), Just adr) -> do
      sz <- genTy ty
      genExpression expr
      let instrs =
            [ "i32.const 1",
              "i32.sub",
              "i32.const" <+> pretty sz,
              "i32.mul",
              "i32.const" <+> pretty adr,
              "i32.add",
              "i32.load"
            ]
      modify $ \cgState@CodegenState {..} ->
        cgState {asm = asm ++ instrs}
      return (Ref ty)
    _ -> error "not a var"

-- | @genConst@ places a constant to be evaluated on the stack
genConst :: Expr -> Codegen ()
genConst e = case e of
  IntConst i ->
    let newInstr = ["i32.const" <+> pretty i]
     in modify $ \cgState@CodegenState {..} ->
          cgState {asm = asm ++ newInstr}
  BoolConst b ->
    let newInstr = ["i32.const" <+> pretty (fromEnum b)]
     in modify $ \cgState@CodegenState {..} ->
          cgState {asm = asm ++ newInstr}

-- | @genExpression@ generates code recursively to evaluate an expression
--    and leaves the result on the stack
genExpression :: Expr -> Codegen SymType
genExpression (BoolConst b) = do
  modify $ \cgState@CodegenState {..} -> cgState {asm = asm ++ ["i32.const" <+> pretty (fromEnum b)]}
  return (Var Boolean)
genExpression (IntConst i) = do
  modify $ \cgState@CodegenState {..} -> cgState {asm = asm ++ ["i32.const" <+> pretty i]}
  return (Var Integer)
genExpression (V d) = loadItem d
genExpression e@(Not _) = genUnaryOp e
genExpression e@(Negate _) = genUnaryOp e
genExpression e@(BBinary _ _ _) = genBinaryOp e
genExpression e@(ABinary _ _ _) = genBinaryOp e
genExpression e@(RelBinary _ _ _) = genRelation e

genUnaryOp :: Expr -> Codegen SymType
genUnaryOp (Not expr) = do
  symTy <- genExpression expr
  case symTy of
    Var Boolean -> do
      let newInstr = "i32.eqz"
      modify $ \cgState@CodegenState {..} ->
        cgState {asm = asm ++ [newInstr]}
      return symTy
    _ -> error "not a valid expression"
genUnaryOp (Negate expr) = do
  symTy <- genExpression expr
  case symTy of
    Var Integer -> do
      let newInstr = ["i32.const -1", "i32.mul"]
      modify $ \cgState@CodegenState {..} ->
        cgState {asm = asm ++ newInstr}
      return symTy
    _ -> error "not a valid expression"
genUnaryOp _ = error "Not a valid expression"

-- | @genBinaryOp@ generates code for x op y; op is either an arithmetic or boolean
--     operator
genBinaryOp :: Expr -> Codegen SymType
genBinaryOp (ABinary op (IntConst a) (IntConst b)) =
  let res = case op of
        Times -> a * b
        Div -> a `div` b
        Mod -> a `mod` b
        Plus -> a + b
        Minus -> a - b
      newInstr = "i32.const" <+> pretty res
   in do
        modify $ \cgState@CodegenState {..} ->
          cgState {asm = asm ++ [newInstr]}
        return (Var Integer)
genBinaryOp (ABinary op e1 e2) = do
  ty1 <- genExpression e1
  ty2 <- genExpression e2
  let isInteger ty = case ty of
        Var ty -> ty == Integer
        _ -> False
      opInstr = case op of
        Times -> "i32.mul"
        Div -> "i32.div_s"
        Mod -> "i32.rem_s"
        Plus -> "i32.add"
        Minus -> "i32.sub"
  -- guard (isInteger ty1 && isInteger ty2)
  modify $ \cgState@CodegenState {..} ->
    cgState {asm = asm ++ [opInstr]}
  return (Var Integer)
genBinaryOp (BBinary op (BoolConst b1) (BoolConst b2)) =
  let res = case op of
        And -> b1 && b2
        Or -> b1 || b2
      newInstr = "i32.const" <+> pretty res
   in do
        modify $ \cgState@CodegenState {..} ->
          cgState {asm = asm ++ [newInstr]}
        return (Var Boolean)
genBinaryOp (BBinary And e1 e2) = do
  ty1 <- genExpression e1
  -- guard (isBoolean ty1)
  modify $ \cgState@CodegenState {..} -> cgState {asm = asm ++ ["if (result i32)"]}
  ty2 <- genExpression e2
  -- guard (isBoolean ty2)
  modify $ \cgState@CodegenState {..} -> cgState {asm = asm ++ ["else", "i32.const 0", "end"]}
  return (Var Boolean)
genBinaryOp (BBinary Or e1 e2) = do
  ty1 <- genExpression e1
  -- guard (isBoolean ty1)
  modify $ \cgState@CodegenState {..} -> cgState {asm = asm ++ ["if (result i32)", "i32.const 1", "else"]}
  ty2 <- genExpression e2
  -- guard (isBoolean ty2)
  modify $ \cgState@CodegenState {..} -> cgState {asm = asm ++ ["end"]}
  return (Var Boolean)

isBoolean ty = case ty of
  Var ty -> ty == Boolean
  _ -> False

genRelation :: Expr -> Codegen SymType
genRelation (RelBinary op e1 e2) = do
  ty1 <- genExpression e1
  ty2 <- genExpression e2
  -- guard (ty1 == ty2)
  let newInstr = case op of
        Equal -> "i32.eq"
        NEqual -> "i32.ne"
        LE -> "i32.lt_s"
        GT -> "i32.gt_s"
        LTE -> "i32.le_s"
        GTE -> "i32.ge_s"
  modify $ \cgState@CodegenState {..} -> cgState {asm = asm ++ [newInstr]}
  return (Var Boolean)

-- | Generates the location to store a variable in an array.
--     Should be called from @genAssign@
-- TODO: Runtime checking on array bounds?
genIndex :: Designator -> Codegen ()
genIndex (Designator v (Just expr)) = do
  (lvl, (symTy, mAdr)) <- fetch v
  case (symTy, mAdr) of
    (Var (Array lb ub ty), Just adr) -> do
      sz <- genTy ty
      genExpression expr
      let instrs =
            [ "i32.const" <+> pretty lb,
              "i32.sub",
              "i32.const" <+> pretty sz,
              "i32.mul",
              "i32.const" <+> pretty adr,
              "i32.add",
              "i32.store"
            ]
      modify $ \cgState@CodegenState {..} ->
        cgState {asm = asm ++ instrs}
    -- return (Ref ty)
    _ -> error "not a var"

-- | Generates code for x := y
genAssign :: Designator -> Codegen ()
genAssign d = do
  cgState@CodegenState {..} <- get
  case d of
    Designator v Nothing -> do
      (lvl, (symTy, _)) <- fetch v
      case lvl of
        Global -> put $ cgState {asm = asm ++ ["global.set $" <> pretty v]}
        l@(Nested i) ->
          if l == curlev
            then put $ cgState {asm = asm ++ ["local.set $" <> pretty v]}
            else error "not in scope"
    idx@(Designator v (Just _)) -> genIndex idx

genProgEntry :: Var -> Codegen ()
genProgEntry name = do
  modify $ \cgState@CodegenState {..} ->
    cgState {curlev = Nested 1, asm = asm ++ ["(func $program"]}

genProgExit :: Var -> Codegen ()
genProgExit v = do
  modify $ \cgState@CodegenState {..} ->
    cgState {curlev = Nested 0, asm = asm ++ ["(memory" <+> pretty (memsize `div` 2 ^ 16 + 1) <> ")", "(start $program)"]}

genProc :: Declaration -> Codegen ()
genProc (ProcDecl name params ret declrs stmts) = do
  cgState@CodegenState {..} <- get
  let pDecs (ps, ty) = hsep $ map (\p -> "param $" <> pretty p <+> "i32") ps
      rDecs (Just (v, ty)) = (parens "result i32")
      rDecs Nothing = ""
      funcLine =
        ("(func $" <> pretty name <+> hsep (map pDecs params) <+> rDecs ret) : if (ret == Nothing) then [] else [""]
      funcDecs = [rDecs ret]
  put $ cgState {asm = asm ++ funcLine ++ funcDecs, curlev = succ curlev}

genProcExit :: Codegen ()
genProcExit = do
  modify $ \cgState@CodegenState {..} ->
    cgState {asm = asm ++ [")"], curlev = pred curlev}

genActualPara :: Designator -> Codegen ()
genActualPara var@(Designator v mExpr) = do
  cgState@CodegenState {..} <- get
  (lvl, (symTy, adr)) <- fetch v
  case lvl of
    l@(Nested i) ->
      if l == curlev
        then put (cgState {asm = asm ++ ["local.set" <+> "$" <> pretty v]})
        else error "Not same level"
    Global -> put (cgState {asm = asm ++ ["global.set" <+> "$" <> pretty v]})
    Memory -> case adr of
      Nothing -> error "not in memory"
      Just adr -> put (cgState {asm = asm ++ ["i32.const" <+> pretty adr]})

genCall :: Statement -> Codegen ()
genCall (ProcCall mDesig name expr) = do
  mapM genExpression expr
  let call = "call" <+> pretty name
  case mDesig of
    Nothing -> modify $ \cgState@CodegenState {..} -> cgState {asm = asm ++ [call]}
    Just d -> do
      modify $ \cgState@CodegenState {..} -> cgState {asm = asm ++ [call]}
      genActualPara d

genRead :: Statement -> Codegen ()
genRead (ProcCall (Just d) "read" []) = do
  modify $ \cgState@CodegenState {..} -> cgState {asm = asm ++ ["call $read"]}
  genAssign d

genWrite :: Codegen ()
genWrite = do
  modify $ \cgState@CodegenState {..} -> cgState {asm = asm ++ ["call $write"]}

genWriteln :: Codegen ()
genWriteln = do
  modify $ \cgState@CodegenState {..} -> cgState {asm = asm ++ ["call $writeln"]}

genThen :: Expr
genThen = undefined
-}
