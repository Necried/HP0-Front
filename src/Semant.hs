{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}

-- |
-- Module        : ST
-- Description   : Symbol table for HP0.
-- Copyright     : (c) Lucas Dutton, 2020
-- License       : GPL-3
-- Maintainer    : duttonl@mcmaster.ca
-- Stability     : experimental
-- Portability   : POSIX
--
-- The symbol table of HP0. It is used during code generation to
-- store information about variables
module Semant where

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity
import Control.Applicative hiding (Const)

import Debug.Trace

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Builder
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Functor
import Data.Function ((&))
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Maybe (isJust, fromJust, listToMaybe, catMaybes)
import Data.Traversable
import Data.Semigroup
import Lens.Micro.Platform
import Witch

import Prelude hiding (GT)
import Syntax
import Utils

-- | Toplevel Symbol Table definitions
data SymKind
  = SymKind
    { ty :: Type
    , kindInfo :: KindInfo
    }
    deriving (Eq, Show)

-- | KindInfo dictates what the variable is at runtime
data KindInfo
  = Var  -- | Global variables, local variables, value parameters (Int or Bool)
  | Ref
      { refTySizes :: [TySize],          -- Sizes of each subarray
        refLowerBounds :: [LowerBound],  -- Lower bounds of each subarray, for codegen
        refBaseAdr :: Adr                -- The base address of the reference
      }  -- | Reference parameters of any type (currently for arrays)
  | Const -- | Int or Bool constants
  | TypeDefKind TySize -- | Type for named or anonymous types
  | ProcedureCall {- [(Var, Type)] (Maybe (Var, Type)) -} -- | declared procedures
  | StdProc -- | native functions defined externally for wasm runtime
  deriving (Eq, Show)

type Adr = Int
type TySize = Int

-- | Level of a declaration
data Level
  = Stack
  | Memory
  | Global
  | Nested Int
  deriving (Eq, Ord, Show)
  deriving (Semigroup, Monoid) via (Max Level)

instance Bounded Level where
  minBound = Global
  maxBound = Nested (maxBound :: Int)

instance Enum Level where
  toEnum 0 = Stack
  toEnum 1 = Memory
  toEnum 2 = Global
  toEnum n = Nested (fromIntegral n - 3)

  fromEnum Stack = 0
  fromEnum Memory = 1
  fromEnum Global = 2
  fromEnum (Nested i) = fromIntegral i - 3

type Name = Text

data SymTab = SymTab
  { _vars :: Map Var SymKind,
    _curLev :: Level
  } deriving (Show)

makeLenses ''SymTab

-- The Semantic AST which is mainly used to store the type
-- of nodes
type SExpr = (SymKind, SExpr')
data SExpr'
  = SSelect Var SDesignator
  | SBoolConst Bool
  | SV SDesignator
  | SNot SExpr
  | SBBinary BBinOp SExpr SExpr
  | SRelBinary RBinOp SExpr SExpr
  | SIntConst Int
  | SStringConst Adr
  | SNegate SExpr
  | SABinary ABinOp SExpr SExpr
  deriving (Eq, Show)

data SDesignator = SDesignator Level Var (Maybe RunTimeIndex)
  deriving (Eq, Show)

data RunTimeIndex = RunTimeIndex
  { indices :: [(SExpr, TySize, LowerBound)]
  , baseAddress :: Adr
  }
  deriving (Eq, Show)

data ProcCallMethod
  = CallDirect
  | CallIndirect
  deriving (Eq, Show)

data SStatement
  = SAssign SDesignator SExpr
  | SProcCall ProcCallMethod (Maybe SDesignator) Var [SExpr]
  | SCompose [SStatement]
  | SIf SExpr [SStatement] (Maybe [SStatement])
  | SWhile SExpr [SStatement]
  deriving (Eq, Show)

data SDeclaration
  = SConstDecl Var Level SExpr
  | STyDecl
  | SVarDecls Level [(Var, SymKind)]
  | SProcDecls Level Var [(Var, SymKind)] (Maybe (Var, SymKind))
      [SDeclaration] [SStatement]
  deriving Show

data SProgram = SProgram
  { metaData :: MetaData,
    topLevels :: [SDeclaration],
    progName :: Var,
    declrs :: [SDeclaration],
    stmts :: [SStatement]
  }
  deriving Show



------------------------------------------
-- Data types used in semantic checking --
------------------------------------------

data StringEntry = StringEntry
  { encodingLength :: Int,
    literalLength :: Int,
    literalBS :: ByteString
  } deriving (Show)

type StringTable = Map Adr StringEntry

type FuncTable = Map Var Int

type TyperefTable = Map Text Type

-- The MetaData is attached to the SProgram, and propogates
-- information such as memory sizes and constant strings
-- to the code generator
data MetaData = MetaData
  { _stringTable :: StringTable,
    _memStrPtr :: Int,
    _funcTable :: FuncTable,
    _typerefTable :: TyperefTable
  } deriving (Show)

makeLenses ''MetaData

data Env = Env
  { _symTabs :: [SymTab],
    _memsize :: Int,
    _strsize :: Int,
    _funcIdxCtr :: Int,
    _constantStrings :: StringTable,
    _funcTableEnv :: FuncTable,
    _typerefTableEnv :: TyperefTable
  } deriving (Show)

makeLenses ''Env

newSymTab :: Level -> SymTab
newSymTab = SymTab Map.empty

initSymTab :: SymTab
initSymTab = SymTab Map.empty Global

{-
We reserve the first 3 32-bits for storing specific information
- mem[0] stores the current memory size
- mem[1] and mem[2] are "registers" which we use when working with
  string concatenation
-}
initEnv :: Env
initEnv = Env
  { _symTabs = [initSymTab],
    _memsize = 12, -- TODO: Store offset zero to point to current empty slot
    _strsize = 4,
    _funcIdxCtr = 0,
    _constantStrings = Map.empty,
    _funcTableEnv = Map.empty,
    _typerefTableEnv = Map.empty
  }

type SemantM a = StateT Env (ExceptT SemantError Identity) a

data SemantError
  = UndefinedSymbol Var String -- The string is temporarily a debug
  | VariableDeclared Var
  | WrongVarKind Var
  | Unimplemented Int
  | TypeMismatch { debugIndex :: Int, expectedType :: [Type], gotType :: Type}
  | KindError { debugIndex :: Int, expectedKind :: [SymKind], gotKind :: SymKind}
  | NotConst { expectedConst :: [SymKind], gotExpr :: Expr }
  | InvalidScope { expectedLv :: Level, gotLv :: Level }
  | ParamListMismatch { procName :: Var, expectedList :: [Type], gotList :: [Expr] }
  | LowerBoundError
  | UpperBoundError
  deriving Show

runSemant :: SemantM SProgram -> Env -> Either SemantError SProgram
runSemant m env = runIdentity $ runExceptT $ evalStateT m env
  -- runExceptT m & flip evalState env
-- Functions on the Symbol Table


{-
instance Semigroup Checker where
  c1 <> c2 =
    Checker
    (symTab c1 `Map.union` symTab c2)
    (memsize c1 + memsize c2)

instance Monoid Checker where
  mempty = Checker Map.empty 0
  -- TODO: Check if `union` is what we want
  mappend = (<>)
-}

instance (Semigroup a) => Semigroup (SemantM a) where
  (<>) = liftA2 (<>)

instance (Monoid a) => Monoid (SemantM a) where
  mempty = pure mempty

----------------------------------------------------------
-- Constant Folding --
----------------------------------------

class ConstFold op where
  applyOp :: op -> SExpr -> SExpr -> SemantM SExpr

instance ConstFold ABinOp where
  applyOp op
    (SymKind Integer Const, SIntConst a)
    (SymKind Integer Const, SIntConst b) =
    pure $ (SymKind Integer Const, ) $ SIntConst $ case op of
      Times -> a * b
      Div   -> a `div` b
      Mod   -> a `mod` b
      Plus  -> a + b
      Minus -> a - b
  applyOp Plus
    (SymKind String Const, SStringConst adr1)
    (SymKind String Const, SStringConst adr2) = do
    stringTable <- use constantStrings
    let mString1 = stringTable ^? ix adr1
        mString2 = stringTable ^? ix adr2
    case (mString1, mString2) of
      (Just (StringEntry eLen1 sLen1 bs1), Just (StringEntry eLen2 sLen2 bs2)) ->
        let newEntry = StringEntry (eLen1 + eLen2) (sLen1 + sLen2) (bs1 <> bs2)
        in do
          baseAdr <- use strsize
          strsize += (eLen1 + eLen2)
          modify $ \env -> env & constantStrings . at baseAdr ?~ newEntry
          pure (SymKind String Const, SStringConst baseAdr)
  applyOp op a b = pure $
    if isSameTy (fst a) (SymKind String Var) && isSameTy (fst b) (SymKind String Var)
      then (SymKind String Var, SABinary op a b)
      else (SymKind Integer Var, SABinary op a b)

instance ConstFold BBinOp where
  applyOp op
    (SymKind Boolean Const, SBoolConst a)
    (SymKind Boolean Const, SBoolConst b) =
    pure $ (SymKind Boolean Const, ) $ SBoolConst $ case op of
      And -> a && b
      Or  -> a || b
  applyOp op a b = pure (SymKind Boolean Var, SBBinary op a b)

instance ConstFold RBinOp where
  applyOp op e1@(ty1, a) e2@(ty2, b) = case ((ty1, ty2), (a, b)) of
    ((SymKind Integer Const, SymKind Integer Const), (SIntConst x, SIntConst y)) ->
      pure (SymKind Boolean Const, SBoolConst $ applyCompare op x y)
    ((SymKind Boolean Const, SymKind Boolean Const), (SBoolConst x, SBoolConst y)) ->
      pure (SymKind Boolean Const, SBoolConst $ applyCompare op x y)
    _ -> pure (SymKind Boolean Const, SRelBinary op e1 e2)
    where
      applyCompare :: (Ord a) => RBinOp -> a -> a -> Bool
      applyCompare op = case op of
        Equal -> (==)
        NEqual -> (/=)
        LE -> (<)
        LTE -> (<=)
        GT -> (>)
        GTE -> (>=)

---------------------------------
-- Utility Functions -----------
--------------------------------

findVar :: Var -> SemantM (Maybe (SymKind, Level))
findVar var = do
  curSymTab <- use symTabs
  let foundVars = fmap (\st ->
        fmap (, st ^. curLev) (Map.lookup var (st ^. vars))) curSymTab
  pure $  foundVars ^? to catMaybes . _head

addDecl :: Var -> SymKind -> SemantM (Var, SymKind)
addDecl var symKind = do
  Env{..} <- get
  varExists var
  modify $ \env -> env & symTabs . _head . vars . at var ?~ symKind
  pure (var, symKind)
  -- \s -> s { symTab = Map.insert (var, lev) symKind (symTab s) }

addDeclAt :: Level -> Var -> SymKind -> SemantM (Var, SymKind)
addDeclAt lvl var symKind = do
  varExists var
  curLevel <- currentScope
  modify $ \env -> env & symTabs . ix 1 . vars . at var ?~ symKind

  symTabs' <- use symTabs
  traceShowM symTabs'
  -- symTabs . ix (fromEnum lvl - 2) . vars . ix var .= symKind
  pure (var, symKind)

varExists :: Var -> SemantM ()
varExists var = do
  Env{..} <- get
  let varDoesNotExist = notElem var $ _symTabs ^.. _head . vars . folding Map.keys
  unless varDoesNotExist $ throwError $  VariableDeclared var

currentScope :: SemantM Level
currentScope = use $ symTabs . _head . curLev

openScope :: SemantM ()
openScope = do
  lvl <- currentScope
  symTabs %= (newSymTab (succ lvl) :)

closeScope :: SemantM ()
closeScope = do
  symTabs %= (\(t:rest) -> rest)

addFuncIdx :: Type -> Var -> SemantM ()
addFuncIdx ty procName = do
  idx <- use funcIdxCtr
  modify $ \env -> env & funcTableEnv . at procName ?~ idx
  funcIdxCtr += 1

addTyperef :: Var -> Var -> Type -> SemantM ()
addTyperef baseName var ty =
  let hashedVal = hashRettyName baseName var
  in modify $ \env -> env & typerefTableEnv . at hashedVal ?~ ty

----------------------------------
-- Semantic Check functions ------
----------------------------------

checkProgram :: Program -> SemantM SProgram
checkProgram (Program topLevels progName declrs stmts) = do
  -- Add default types/functions
  addDecl "read" $ SymKind (Func [] $ Just Integer) StdProc
  addDecl "writeln" $ SymKind (Func [] Nothing) StdProc
  addDecl "write" $ SymKind (Func [Integer] Nothing) StdProc
  addDecl "writestr" $ SymKind (Func [String] Nothing) StdProc
  sTopLevels <- mapM checkDeclr topLevels
  openScope
  progDeclrs <- mapM checkDeclr declrs
  progStmts <- mapM checkStmt stmts
  closeScope
  stringEntries <- use constantStrings
  strPtr <- use strsize
  funcTab <- use funcTableEnv
  typerefTab <- use typerefTableEnv
  pure $
    SProgram
    (MetaData stringEntries strPtr funcTab typerefTab)
    sTopLevels
    progName
    progDeclrs
    progStmts
  -- analyze (Program globals progName progDeclrs progStmts) =
  --  sequence [analyze globals, analyze progDeclrs, analyze progStmts] <&> mconcat

getBaseType :: Type -> SemantM Type
getBaseType (Typedef v) = do
  mVarInfo <- findVar v
  case mVarInfo of
    Just (SymKind ty _, _) -> getBaseType ty
    _ -> throwError $ Unimplemented 308
getBaseType (Array _ _ ty) = getBaseType ty
getBaseType ty = pure ty

-- TODO: isSameTy could be the Eq instance for SymKind?
-- TODO: Nope, have to convert function vars into procedure calls
isSameTy :: SymKind -> SymKind -> Bool
isSameTy (SymKind t1 _) (SymKind t2 _) = t1 == t2

cmpSymTy :: SymKind -> Type -> Bool
cmpSymTy (SymKind t1 _) t2 = t1 == t2

createArrayRefType' :: Adr -> Type -> SemantM (SymKind, TySize)
createArrayRefType' adr (Array lb ub ty) = do
  (symKind,prevSize)  <- createArrayRefType' adr ty
  pure $ case symKind of
    (SymKind ty (Ref tySize prevlb _)) ->
      let curSize = head tySize * prevSize
      in (SymKind ty (Ref (curSize:tySize) (lb:prevlb) adr), curSize * (ub - lb + 1))
    (SymKind ty Var) ->
      (SymKind ty (Ref [prevSize] [lb] adr), ub - lb + 1)
createArrayRefType' adr ty@(Typedef _) = do
  ty' <- unpackTypedef ty
  createArrayRefType' adr ty'
createArrayRefType' adr Boolean = pure (SymKind Boolean Var, 1)
createArrayRefType' adr Integer = pure (SymKind Integer Var, 4)

createArrayRefType :: Adr -> Type -> SemantM SymKind
createArrayRefType adr ty = fst <$> createArrayRefType' adr ty

-- PRECONDITION: Typedefs has been folded away
collectLowerBounds :: Type -> [LowerBound]
collectLowerBounds (Array lb ub ty) = lb : collectLowerBounds ty
collectLowerBounds _ = []

unpackTypedef :: Type -> SemantM Type
unpackTypedef (Array lb ub ty) = Array lb ub <$> unpackTypedef ty
unpackTypedef (Typedef var) = do
  varInfo <- findVar var
  case varInfo of
    Just (SymKind ty' _, _) -> unpackTypedef ty'
    _ -> throwError $ UndefinedSymbol var "unpackTypedef"
unpackTypedef ty = pure ty

checkIfArray :: Type -> SemantM ()
checkIfArray (Array lb ub ty) = do
  when (lb < 0) $ throwError LowerBoundError
  when (ub < lb) $ throwError UpperBoundError
  checkIfArray ty
checkIfArray (Typedef v) = do
  mSymKind <- findVar v
  case mSymKind of
    Just (SymKind ty kind, _) -> checkIfArray ty
    Nothing -> throwError $ UndefinedSymbol v "checkIfArray"
    Just _ -> throwError $ WrongVarKind v
checkIfArray _ = pure ()

typeSize :: Type -> SemantM TySize
typeSize Integer = pure 4
typeSize Boolean = pure 1
typeSize (Array lb ub ty) = typeSize ty
typeSize (Typedef v) = do
  mTy <- findVar v
  case mTy of
    Just (SymKind ty kind, _) -> typeSize ty
    Nothing -> throwError $ UndefinedSymbol v "typeSize"
    Just _ -> throwError $ WrongVarKind v

arraySize :: Type -> SemantM TySize
arraySize (Array lb ub ty) = (*) <$> arraySize ty <*> pure (ub - lb + 1)
arraySize (Typedef v) = do
  mTy <- findVar v
  case mTy of
    Just (SymKind ty kind, _) -> arraySize ty
    Just _ -> throwError $ WrongVarKind v
    Nothing -> throwError $ UndefinedSymbol v "arraySize"
arraySize t = typeSize t

allocArrayMem :: Type -> SemantM Adr
allocArrayMem ty@Array {} = do
  tySize <- arraySize ty
  curMemSize <- use memsize
  memsize += tySize
  pure curMemSize
alloArrayMem ty = error $ "Trying to call alloArrayMem on " ++ show ty

allocStrMem :: String -> SemantM Adr
allocStrMem s = do
  baseAdr <- use strsize
  -- At runtime, in addition to the string we store
  -- the length of the string in the first 4 bytes
  -- (The length is the number of unicode codepoints),
  -- i.e. a smiley face has 4 codepoints, so the length is 4
  -- and not 1
  -- We also store the actual string length right after
  -- the encoding length
  strsize += (eLen + 8)
  -- We have to propagate base address so we know where to
  -- initialize the string in the header, and where to access
  -- when called in an expression
  modify $ \env -> env & constantStrings . at baseAdr ?~ StringEntry eLen sLen bs
  pure baseAdr
  where
    -- Since HP0 supports UTF-8, we uniformly encode strings as a sequence of bytes;
    -- each byte is a literal hexadecimal character, and putting two of these
    -- together makes up a semantic codepoint (hence the `div` 2 in eLen)
    bs = toStrict $ toLazyByteString $ byteStringHex $ T.encodeUtf8 $ T.pack s
    eLen = BS.length bs `div` 2
    sLen = length s

addVarDecl :: Type -> Var -> SemantM (Var, SymKind)
addVarDecl arr@(Array _ _ ty') v= do
  checkIfArray arr
  adr <- allocArrayMem arr
  createArrayRefType adr arr >>= addDecl v
addVarDecl ty@(Typedef tyVar) v = do
  unpackedTy <- unpackTypedef ty
  addVarDecl unpackedTy v
addVarDecl ty v = addDecl v $ SymKind ty Var

checkDeclr :: Declaration -> SemantM SDeclaration
checkDeclr declrs =
  let
    constExpr :: SExpr -> Bool
    constExpr (SymKind _ Const, _) = True
    constExpr _ = False

    pRemoveRefs :: SymKind -> Bool
    pRemoveRefs (SymKind _ Ref {}) = False
    pRemoveRefs _ = True
  in case declrs of
    ConstDecl varName expr -> do
      Env{..} <- get
      sexpr <- checkExpr expr
      unless (constExpr sexpr) $ throwError $ NotConst [fst sexpr] expr
      (var, symKind) <- addDecl varName $ fst sexpr
      curLev' <- currentScope
      pure $ SConstDecl var curLev' sexpr
    -- Type declarations could be arrays, e.g. `type T = 1..5 → Int`.
    -- For this case, we need to do a few things:
    --   1. Check array bounds
    --   2. Figure out the array size which has to be stored in the Type SymKind
    --   3. Create a mapping from the type variable to the definition
    -- Note that in the code generation phase, this information is completely
    -- erased, as the SAST will already contain the absolute address in memory
    -- if its an array, or completely ignore simple (Int/Bool) typedefs
    TyDecl varName ty -> do
      curLev' <- currentScope
      unless (curLev' == Global) $ throwError $ InvalidScope Global curLev'
      checkIfArray ty
      tySize <- arraySize ty
      void $ addDecl varName (SymKind ty (TypeDefKind tySize))
      pure STyDecl
    -- Variable declarations are either simple type bindings, reference types like
    -- arrays, or typedefs.
    -- - For simple type bindings, we just add information to the symbol table
    -- - For arrays, we have to calculate the memory size and place the base address
    -- in the SymKind which is then used later for calculating indices in codegen
    -- - Typedefs need to look into the symbol table to fetch type information,
    -- and it will eventually simplify into the two cases above.
    VarDecls declList -> do
      curLev' <- currentScope
      decls <- forM declList $ \(vars, ty) ->
        filterM (pure . pRemoveRefs . snd) =<< forM vars (addVarDecl ty)
      pure $ SVarDecls curLev' $ concat decls
    ProcDecl fname params retty declrs stmts -> do
      -- TODO: When we add nested procedures, level has to be inferred
      -- Immediately put the function name in scope
      -- This deals with mutual recursion (I think?)
      addDecl fname (SymKind (Func [] Nothing) ProcedureCall)
      openScope
      varDecls <- forM params $ \(vars, ty) ->
        forM vars $ \var -> do
        -- Any variables which are function pointers must have declared
        -- return types, so when call_indirects are used in WebAssembly,
        -- we can dynamically pass the type at the call site
        case ty of
          Func inputs mOutput -> addTyperef fname var ty
          _ -> pure ()
        addDecl var (SymKind ty Var)
      rettyDecl <- forM retty $ \(var, ty) -> addDecl var (SymKind ty Var)

      -- re-add the function with updated procCalls in the previous scope
      curScope <- currentScope
      let funcTy = Func (ty . snd <$> concat varDecls) (ty . snd <$> rettyDecl)
      addDeclAt (pred curScope) fname
        (SymKind funcTy ProcedureCall)
      sdecls <- mapM checkDeclr declrs
      sstmts <- mapM checkStmt stmts
      functionScope <- currentScope
      closeScope

      -- Store a pointer to the function in the function table, allowing
      -- for first-class function access
      addFuncIdx funcTy fname

      pure $ SProcDecls functionScope fname (concat varDecls) rettyDecl sdecls sstmts

checkStmt :: Statement -> SemantM SStatement
checkStmt (Compose stmts) = SCompose <$> mapM checkStmt stmts
checkStmt (Assign desig expr) = do
  -- TODO: What if it's not a Var?
  (t1, sDesig)  <- checkDesig desig
  rhs@(t2, _) <- checkExpr expr
  Env{..} <- get
  unless (isSameTy t1 t2) $ traceShow _symTabs $ throwError $ KindError 1 [t1] t2
  return $ SAssign sDesig rhs
checkStmt (If pred thenStmt mElseStmt) = do
  pred'@(ty, _) <- checkExpr pred
  unless (ty == SymKind Boolean Var || ty == SymKind Boolean Const) $ throwError $
    KindError 2 [SymKind Boolean Var, SymKind Boolean Const] ty
  SIf pred' <$> mapM checkStmt thenStmt <*> mapM (mapM checkStmt) mElseStmt
checkStmt (While cond stmts) = do
  cond'@(ty, _) <- checkExpr cond
  unless (ty == SymKind Boolean Var || ty == SymKind Boolean Const) $ throwError $
    KindError 3 [SymKind Boolean Var, SymKind Boolean Const] ty
  SWhile cond' <$> mapM checkStmt stmts
checkStmt (ProcCall mDesig procName exprs) = do
  mSDesig <- traverse checkDesig mDesig -- lift checkDesig <$> mDesig
  mVarInfo <- findVar procName
  case mVarInfo of
    Just (SymKind (Func params retty) ProcedureCall, lvl) -> do
      -- Check that actuals have same length as formals
      unless (length params == length exprs) $ throwError $
        ParamListMismatch procName params exprs

      -- Check return type matches
      unless ((ty . fst <$> mSDesig) == retty) $ throwError $
        KindError 4 [SymKind (fromJust retty) Var] $ fst (fromJust mSDesig)

      {-
      case mSDesig of
        Nothing -> pure ()
        -- TODO allow array-indexed functions in the future
        Just (ty, SDesignator lvl' v Nothing) ->
          unless
      -}

      sActuals <- forM (zip params exprs) $ \(formal, actual) -> do
        sActual@(tyActual, sexpr) <- checkExpr actual
        unless (formal == ty tyActual) $ throwError $
          TypeMismatch 1 [formal] $ ty tyActual
        pure sActual

      pure $ SProcCall CallDirect (snd <$> mSDesig) procName sActuals

    Just (SymKind (Func argTys mRetType) StdProc, _) -> do
      -- Check that actuals have same length as formals
      unless (length argTys == length exprs) $ throwError $
        ParamListMismatch procName argTys exprs

      -- Check return type matches
      unless ((ty . fst <$> mSDesig) == mRetType) $ throwError $
        TypeMismatch 2 [fromJust mRetType] (ty . fst . fromJust $ mSDesig)


      sActuals <- forM (zip argTys exprs) $ \(kind, actual) -> do
        sActual@(tyActual, sexpr) <- checkExpr actual
        unless (ty tyActual == kind) $ throwError $
          TypeMismatch 3 [ty tyActual] kind
        pure sActual

      pure $ SProcCall CallDirect (snd <$> mSDesig) procName sActuals

    -- For functions as variables
    Just (SymKind (Func inputs output) Var, lvl) -> do
      -- Check that actuals have same length as inputs
      unless (length inputs == length exprs) $ throwError $
        ParamListMismatch procName inputs exprs

      -- Check input exprs match formal types
      sActuals <- forM (zip inputs exprs) $ \(inpTy, actual) -> do
        sActual@(tyActual, sexpr) <- checkExpr actual
        unless (ty tyActual == inpTy) $ throwError $
          TypeMismatch 4 [ty tyActual] inpTy
        pure sActual

      -- Check return type matches
      unless ((ty . fst <$> mSDesig) == output) $ throwError $
        TypeMismatch 5 [fromJust output] (ty . fst . fromJust $ mSDesig)

      pure $ SProcCall CallIndirect (snd <$> mSDesig) procName sActuals
    _ -> do
      symTabs' <- gets _symTabs
      throwError $ UndefinedSymbol procName "checkStmt"

checkExpr :: Expr -> SemantM SExpr
checkExpr (RelBinary op e1 e2) = do
  lhs@(ty1, _) <- checkExpr e1
  rhs@(ty2, _) <- checkExpr e2
  if not $ isSameTy ty1 ty2 -- TODO: Only allow relational equalities for integers and booleans?
    then throwError $ KindError 8 [ty1] ty2
    else applyOp op lhs rhs
checkExpr (ABinary op e1 e2) = do
  lhs@(ty1,_) <- checkExpr e1
  rhs@(ty2,_) <- checkExpr e2
  -- TODO: Think of how to fit string concatenation here
  if isSameTy ty1 ty2 {- && cmpSymTy ty2 Integer -- TODO: are Ref Ints allowed? -}
    then applyOp op lhs rhs
    else throwError $ KindError 9 [ty1] ty2
checkExpr (V desig) = do
  (kind, desig) <- checkDesig desig
  pure $ (kind, SV desig)
checkExpr (BBinary op e1 e2) = do
  lhs@(ty1,_) <- checkExpr e1
  rhs@(ty2,_) <- checkExpr e2
  if isSameTy ty1 ty2 && cmpSymTy ty2 Integer
    then applyOp op lhs rhs
    else throwError $ KindError 10 [ty1] ty2
checkExpr (IntConst x) = pure $ (SymKind Integer Const, SIntConst x)
checkExpr (BoolConst b) = pure $ (SymKind Boolean Const, SBoolConst b)
checkExpr (StringConst s) = do
  adr <- allocStrMem s
  pure (SymKind String Const, SStringConst adr)
checkExpr expr = error $ "non exhaustive: " ++ show expr

-- | checkDesig checks validity of variables with potential array indexing
--   If there is indexing, we need to propogate runtime information such as
--    (1) The index expression, and whether it is a constant
--        to enable optimization
--    (2) Array sizes
--    (3) Array lower bounds
--   All these works with nested arrays too, hence (2) and (3) are passed
--   as lists
checkDesig :: Designator -> SemantM (SymKind, SDesignator)
checkDesig (Designator varName Nothing) = do
  Env{..} <- get
  mfoundVar <- findVar varName
  case mfoundVar of
    Nothing -> throwError $ traceShow _symTabs $ UndefinedSymbol varName "checkDesig Nothing"
    Just (ty, lvl) -> pure (ty, SDesignator lvl varName Nothing)
    _ -> throwError $ Unimplemented 532
checkDesig (Designator varName (Just exprs)) = do
  Env{..} <- get
  mfoundVar <- findVar varName
  sexprs <- traverse checkExpr exprs
  unless (all (`cmpSymTy` Integer) $ fmap fst sexprs)
    $ throwError $
    KindError 11 [SymKind Integer Const, SymKind Integer Var]
    (fst $ head sexprs)
  case mfoundVar of
    Nothing -> throwError $ traceShow _symTabs $ UndefinedSymbol varName "checkDesig Just"
    Just (kind@(SymKind ty (Ref tySizes lbs adr)), lvl) ->
      pure (SymKind ty Var, SDesignator lvl varName
             (Just $ RunTimeIndex (zip3 sexprs tySizes lbs) adr))
    _ -> throwError $ traceShow mfoundVar $ Unimplemented 541
