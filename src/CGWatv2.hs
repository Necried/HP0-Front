{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module CGWatv2 where

import Control.Applicative hiding (Const)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer

import Data.List.Split
import Data.Map.Strict ((!), Map)
import qualified Data.Map.Strict as Map
import Data.Function
import Data.Maybe
import Data.Text (Text)
import Data.List
import qualified Data.Text as Text
import qualified Data.ByteString as BS

import Lens.Micro.Platform

import Numeric
import Prettyprinter
import Prelude hiding (GT)
import Witch

import Debug.Trace

import Semant
import Syntax
import Utils

data Context = Context
  { nesting :: Int,
    indentation :: Int,
    scopeName :: Maybe Var,
    funcTableIdx :: FuncTable
  } deriving (Show)

type WASMCode = Doc ()

data Access = Get | Set (CodeGen ())

instance Show Access where
  show Get = "Get"
  show (Set _) = "Set"

newtype CodeGen a = CodeGen (ReaderT Context (Writer WASMCode) a)
  deriving (Functor, Applicative, Monad, MonadReader Context, MonadWriter WASMCode)

initContext :: FuncTable -> Context
initContext = Context 0 0 Nothing

runCodeGen :: FuncTable -> CodeGen a -> (a, WASMCode)
runCodeGen fTable (CodeGen m) =
  runReaderT m (initContext fTable) & runWriter

-- Utility codegen functions

-- | Write a single line of instruction with arguments
writeImm :: Pretty a => Text -> a -> CodeGen ()
writeImm code imm = do
  amount <- asks indentation
  tell $ pretty (replicate amount ' ')
  tell $ pretty code <+> pretty imm <> line

-- | Write a single line of instruction
writeLine :: Text -> CodeGen ()
writeLine code = do
  amount <- asks indentation
  tell $ pretty (replicate amount ' ')
  tell $ pretty code <> line

-- | Modify indentation for a context
withIndent :: Int -> CodeGen () -> CodeGen ()
withIndent inc = local (\r -> r { indentation = inc + indentation r })

-- | Modify the current function scope we're working in
-- Currently used for generating hashes for indirect function calls
withFuncScope :: Var -> CodeGen () -> CodeGen ()
withFuncScope v = local (\r -> r { scopeName = Just v})

-- The memory of a WebAssembly module is segregated into pages; one page is 65536 bytes
-- For now, we reserve page[0] to store arrays, and page[1] for strings
pageSize :: Int
pageSize = 65536

-- TEST
-- In functions with multiple instructions, things can expand real quick and start
-- becoming incomprehensible without a good mental model and good comments
-- This Num instance serves to make code look more readable
-- by abstracting away trivial arithmetic while compiling down to the
-- same machine instructions
instance Num (CodeGen ()) where
  x + y = x >> y >> writeLine "i32.add"
  x * y = x >> y >> writeLine "i32.mul"
  x - y = x >> y >> writeLine "i32.sub"
  fromInteger n = writeImm "i32.const" n
  abs n = undefined

loadFrom :: Integer -> CodeGen ()
loadFrom x = fromInteger @(CodeGen ()) x >> writeLine "i32.load"

load :: CodeGen () -> CodeGen ()
load w = w >> writeLine "i32.load"

store :: CodeGen () -> CodeGen () -> CodeGen ()
store ptr expr = ptr >> expr >> writeLine "i32.store"

-- | Generate the appropriate locals
genVarLevel :: Level -> Var -> CodeGen ()
genVarLevel Global var = writeLine $ "(global $" <> var <> " (mut i32) i32.const 0)"
genVarLevel (Nested _) var = writeLine $ "(local $" <> var <> " i32)"

-- | Generate the get/set ops, based on level
genAccessor :: Access -> Level -> Var -> CodeGen ()
genAccessor access lvl var =
  let
    genLvl = if lvl == Global then "global" else "local"
    genAccess = case access of Get -> "get"; Set _ -> "set"
    accessOp = writeLine $ genLvl <> "." <> genAccess <> " $" <> var
  in case access of
    Get -> accessOp
    Set exprCode -> exprCode >> accessOp


{-
String Concatenation:
on the stack: adr(str1), adr(str2) < top of stack
Then, from top of stack to bottom:
size(str1) = mem[mem[4]]
src(str1)  = mem[4]
dest(str1) = mem[65536] + 8
size(str2) = mem[mem[8]]
src(str2)  = mem[8]
dest(str2) = mem[65536] + size(str1) - 8
str1[0]    = mem[mem[4]]
str2[0]    = mem[mem[8]]
destAdr    = mem[65536]
str1[1]    = mem[mem[4] + 4]
str2[1]    = mem[mem[8] + 4]
destAdrPlus4 = mem[65536] + 4
destAdr (this is the return) = mem[65536] + destAdr[0] + 8

With these on the stack, we call the following sequence of instructions:
memory.copy
memory.copy
i32.add
i32.store
i32.add
i32.store


-}
genExpr :: SExpr -> CodeGen ()
genExpr (_, SIntConst i) = writeImm "i32.const" i
genExpr (_, SBoolConst b) = writeImm "i32.const" $ fromEnum b
genExpr (SymKind String Var, SABinary Plus s1 s2) =
  let
    finalDestAdr =
      store 65536
      $ loadFrom 65536 + load (loadFrom 65536) + 8
    destAdrPlus4 = loadFrom 65536 + 4
    str2Idx1     = load (loadFrom 8 + 4)
    str1Idx1     = load (loadFrom 4 + 4)
    destAdr      = loadFrom 65536
    str2Idx0     = load (loadFrom 8)
    str1Idx0     = load (loadFrom 4)
    destStr2     = loadFrom 65536 + load (loadFrom 4) + 8
    srcStr2      = loadFrom 8 + 8
    sizeStr2     = load (loadFrom 8)
    destStr1     = loadFrom 65536 + 8
    srcStr1      = loadFrom 4 + 8
    sizeStr1     = load (loadFrom 4)

    -- debugLoc n = loadFrom n >> writeLine "call $write" >> writeLine "call $writeln"
  in do
    store 4 $ genExpr s1
    store 8 $ genExpr s2

    -- this is the bottom of the stack!
    -- i32.store
    destAdrPlus4
    -- i32.add
    str2Idx1
    str1Idx1
    -- i32.store
    destAdr
    -- i32.add
    str2Idx0
    str1Idx0
    -- memory.copy
    destStr2
    srcStr2
    sizeStr2
    -- memory.copy
    destStr1
    srcStr1
    sizeStr1

    mapM_ writeLine
      [ "memory.copy"
      , "memory.copy"
      , "i32.add"
      , "i32.store"
      , "i32.add"
      , "i32.store"
      ]

    loadFrom 65536
    finalDestAdr

  -- debugLoc 65596
{-
  -- push dest1 <- mem[65536] + 8
  -- (since we want to reserve the first 8 bytes for string metadata)
  loadFrom 65536 + 8
  -- push src1 <- addr(s1) + 8
  genExpr s1 + 8
  -- push size1 <- s1[0]
  load (genExpr s1)
  -- copy(dest1, src1, size1)
  writeLine "memory.copy"

  -- push dest2 <- mem[65536] + s1 - 65535 + 8
  -- This is basically destAddr + size(dest1)
  loadFrom 65536 + genExpr s1 - 65527
  -- push src2 <- addr(s2) + 8
  genExpr s2 + 8
  -- push size2  <- s2[0]
  load (genExpr s2)
  -- copy(dest2, src2, size2)
  writeLine "memory.copy"
  -- replicateM_ 3 $ sequence [writeLine "call $write", writeLine "call $writeln"]

  -- dest[0] := s1[0] + s2[0]
  store (loadFrom 65536)
    $ load (genExpr s1) + load (genExpr s2)

  -- dest[1] := s1[1] + s2[1] (split in multiple steps)
  store (loadFrom 65536 + 4)
    $ load (genExpr s1 + 4) + load (genExpr s2 + 4)

  -- This step places the address of the new string on the stack
  -- before the increment step
  loadFrom 65536

  -- Now we perform the increment step
  -- mem[65536] = mem[65536] + dest[0] + 8
  -- load destination mem[65536]
  store 65536
    $ loadFrom 65536 + load (loadFrom 65536) + 8
-}
genExpr (_, SABinary op e1 e2) = genExpr e1 >> genExpr e2 >> genArith op
genExpr (_, SRelBinary op e1 e2) = genExpr e1 >> genExpr e2 >> genRelation op
genExpr (_, SBBinary op e1 e2) =
  let ifCode   = if op == And then genExpr e2 else writeImm "i32.const" $ as @Int 1
      elseCode = if op == And then writeImm "i32.const" (as @Int 0) else genExpr e2
  in do
    genExpr e1
    writeLine "if (result i32)"
    withIndent 2 ifCode
    writeLine "else"
    withIndent 2 elseCode
    writeLine "end"
genExpr (SymKind _ ProcedureCall, SV (SDesignator _ fName _)) = do
  funcTable' <- asks funcTableIdx
  writeImm "i32.const" $ funcTable' Map.!? fName
genExpr (_, SV desig) = do
  genDesignator Get desig
genExpr (_, SStringConst adr) = do
  writeImm "i32.const" (adr + pageSize)

genArith :: ABinOp -> CodeGen ()
genArith Times = writeLine "i32.mul"
genArith Plus  = writeLine "i32.add"
genArith Minus = writeLine "i32.sub"
genArith Div   = writeLine "i32.div_s"
genArith Mod   = writeLine "i32.rem_s"

genRelation :: RBinOp -> CodeGen ()
genRelation op = writeLine $ case op of
  Equal -> "i32.eq"
  NEqual -> "i32.ne"
  LE -> "i32.lt_s"
  GT -> "i32.gt_s"
  LTE -> "i32.le_s"
  GTE -> "i32.ge_s"

genStatement :: SStatement -> CodeGen ()
genStatement (SCompose stmts) = mapM_ genStatement stmts
genStatement (SIf pred thenStmt mElseStmt) = do
  genExpr pred
  writeLine "if"
  withIndent 2 $ mapM_ genStatement thenStmt
  case mElseStmt of
    Nothing -> writeLine "end"
    Just elseStmt -> do
      writeLine "else"
      withIndent 2 $ mapM_ genStatement elseStmt
      writeLine "end"
genStatement (SWhile cond stmts) = do
  writeLine "loop"
  withIndent 2 $ do
    genExpr cond
    writeLine "if"
    withIndent 2 $ do
      mapM_ genStatement stmts
      writeLine "br 1"
    writeLine "end"
  writeLine "end"
genStatement (SAssign desig expr) = do
  genDesignator (Set $ genExpr expr) desig
genStatement (SProcCall callMethod mDesig procName sexprs) =
  let
    directFunctionCall = do
      mapM_ genExpr sexprs
      writeLine $ "call $" <> procName
    indirectFunctionCall = do
      mapM_ genExpr sexprs
      -- TODO: This may not be a locally passed function parameter!
      scopeName' <- asks scopeName
      let retName = maybe "" (`hashRettyName` procName) scopeName'
      writeLine $ "(call_indirect (type $" <> retName <> ") (local.get $" <> procName <> "))"
    functionCall = case callMethod of
      CallDirect -> directFunctionCall
      CallIndirect -> indirectFunctionCall
  in maybe functionCall (genDesignator $ Set functionCall) mDesig
genStatement _ = error "unimplemented"

-- | Generate get/set ops of arrays
-- Since i32.store specifically takes the constant on the top of the stack
-- we have to generate the array access first before the expression code
genArrayAccessor :: Var -> TySize -> LowerBound -> CodeGen ()
genArrayAccessor var sz lb = do
  writeImm "i32.const" lb
  writeLine "i32.sub"
  writeImm "i32.const" sz
  writeLine "i32.mul"

-- | For now, genDesignator assumes all are global variables, and of type int
--   and assumes also simple variables only
genDesignator :: Access -> SDesignator -> CodeGen ()
genDesignator access (SDesignator level var Nothing) =
  genAccessor access level var
genDesignator access (SDesignator level var (Just (RunTimeIndex sexprs adr))) =
  let
    getOp = writeLine "i32.load"
    setOp = writeLine "i32.store"
    addressOffset = do
      writeImm "i32.const" adr
      writeLine "i32.add"

    genExprAndAccess ((SymKind Integer Var, SIntConst idx),tySize,lb) =
      let constIdx = (idx - lb) * tySize
      in  writeImm "i32.const" constIdx
    genExprAndAccess (sexpr,tySize,lb) =
      genExpr sexpr >> genArrayAccessor var tySize lb

  -- Why the uncons to extract the head here?
  -- Say we have A[x][y]. The way to generate the indexing is:
  --     (x - lowerBound(yArray)) * sz(yArray) + address(A)  -- (1)
  --   + (y - lowerBound(baseType)) * sz(baseType)           -- (2)
  -- For (1), we have to also take into account the base address of the array
  -- For (2) onwards, we only need to generate for the indexing
  in do
    case uncons sexprs of
      Nothing -> addressOffset
      Just (headExpr, restExpr) -> do
        genExprAndAccess headExpr
        addressOffset
        forM_ restExpr $ \genInfo -> do
          genExprAndAccess genInfo
          writeLine "i32.add"
    case access of
      Get -> getOp
      Set exprCode -> exprCode >> setOp

genDeclr :: SDeclaration -> CodeGen ()
genDeclr (SConstDecl var lev sexpr) = do
  writeLine $ "(local $" <> var <> " i32)"
  genExpr sexpr
genDeclr (SVarDecls lvl vars) =
  mapM_ (genVarLevel lvl . fst) vars
genDeclr STyDecl = pure ()
genDeclr (SProcDecls lvl fname params retty sdecls sstmts) = withFuncScope fname $ do
  writeLine $ funcHeader <> paramList <> retGen
  withIndent 2 $ do
    fromMaybe (pure ()) retLocals
    mapM_ genDeclr sdecls
    mapM_ genStatement sstmts
    case retty of
      Just (retVar, _) -> genAccessor Get lvl retVar
      Nothing -> pure ()
  writeLine ")"
  where
    funcHeader = "(func $" <> fname <> " "
    paramList = Text.intercalate " " $
      (\(v, _) -> "(param $" <> v <> " i32)") <$> params
    retGen = maybe "" (const " (result i32)") retty
    retLocals = fmap (\(v, _) -> genVarLevel lvl v) retty

genConstStrings :: MetaData -> CodeGen ()
genConstStrings metaData = do
  genNextStrAdr
  mapM_ genConstString $ Map.toList (metaData ^. stringTable)
  where
    padBS s = replicate (8 - from @Int (length s)) '0' <> s
    escapeHex s = Text.pack $ "\\" <> intercalate "\\" (chunksOf 2 s)
    -- WebAssembly is little-endian; hence we must reverse the bytes
    escapeNumHex s =
      Text.pack $ "\\" <> intercalate "\\" (reverse $ chunksOf 2 s)
    toHex n = showHex n ""
    -- This encodes the string metadata ints into sequences of bytestrings;
    -- we convert the int to a hex number, pad it to fit within 32-bits,
    -- then escape each byte boundary in accordance to the WebAssembly standard
    -- See https://webassembly.github.io/spec/core/text/values.html#text-string
    -- for more information
    encodeNum = escapeNumHex . padBS . toHex
    -- The hard work of encoding literal strings were done in Semant.hs;
    -- we only have to strip the quotes from the Show instance of the bytestring
    stripQuotes = init . tail

    genNextStrAdr = writeLine $
      "(data (i32.const 65536) \""
      <> encodeNum (metaData ^. memStrPtr + pageSize) <> "\")"

    genConstString (adr, StringEntry eLen sLen bs) =
      let paddedAdr = adr + pageSize
      in writeLine $
         "(data (i32.const " <> (from @String $ show paddedAdr) <> ") \""
         <> encodeNum eLen
         <> encodeNum sLen
         <> escapeHex (stripQuotes $ show bs)
         <> "\")"

genFuncTable :: FuncTable -> CodeGen ()
genFuncTable funcTable = do
  makeTableHeader
  putTableElems
  where
    makeTableHeader =
      writeLine $ "(table " <>  from @String (show $ Map.size funcTable) <> " funcref)"

    putTableElems =
      writeLine $ "(elem (i32.const 0) " <> funcNameList <> ")"

    funcNameList = Text.unwords . map ("$" <>) $ Map.keys funcTable

-- TODO: This may have to be reworked to use more refined types, i.e.
-- only deal with function types
genTypeUse :: Var -> Type -> CodeGen ()
genTypeUse fName (Func inputs mOutput) =
  let
    inpParam = const "(param i32)"
    inpParams = Text.unwords $ map inpParam inputs

    mOutParam = maybe "" (const "(result i32)") mOutput
  in
    writeLine $
      "(type $" <> fName <> " "
      <> "(func " <> inpParams
      <> " " <> mOutParam <> "))"

genProgram :: SProgram -> CodeGen ()
genProgram (SProgram metaData topLevels progName declrs stmts) = do
  genConstStrings metaData
  genFuncTable (metaData ^. funcTable)
  mapM_ (uncurry genTypeUse) $ Map.toList $ metaData ^. typerefTable
  mapM_ genDeclr topLevels
  writeLine "(func $program"
  withIndent 2 $ do
    mapM_ genDeclr declrs
    mapM_ genStatement stmts
  writeLine ")"
  writeLine "(memory $memory 2)"
  writeLine "(export \"memory\" (memory $memory))"
  writeLine "(export \"program\" (func $program))"

genWASM :: SProgram -> CodeGen ()
genWASM prog = do
  writeLine "(module"
  writeLine "  (import \"P0lib\" \"write\" (func $write (param i32)))"
  writeLine "  (import \"P0lib\" \"writeln\" (func $writeln))"
  writeLine "  (import \"P0lib\" \"read\" (func $read (result i32)))"
  writeLine "  (import \"P0lib\" \"writestr\" (func $writestr (param i32)))"
  withIndent 2 $ genProgram prog
  writeLine ")"
