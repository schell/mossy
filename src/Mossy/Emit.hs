{-# LANGUAGE LambdaCase #-}
module Mossy.Emit where

import           Control.Monad             (void)
import           Data.String               (fromString)
--import qualified LLVM.AST                  as AST
--import qualified LLVM.AST.Constant         as C
--import qualified LLVM.AST.IntegerPredicate as IP
--import qualified LLVM.AST.Type             as T
import Language.GLSL

import           Mossy.Codegen
import           Mossy.JIT
import           Mossy.Syntax              as S

--constantTrue :: AST.Operand
--constantTrue  = constant $ C.Int 1 1
--
--constantFalse :: AST.Operand
--constantFalse = constant $ C.Int 1 0
--
--constantZero :: AST.Operand
--constantZero  = constant $ C.Int 32 0
--
--constantOne :: AST.Operand
--constantOne  = constant $ C.Int 32 1
--
--cgen :: S.Expr -> Codegen AST.Operand
--cgen = \case
--  S.Tr     -> return constantTrue
--  S.Fl     -> return constantFalse
--  S.Zero   -> return constantZero
--  S.IsZero expr -> do
--    x <- cgen expr
--    icmp IP.EQ x constantZero
--  S.Succ expr -> do
--    x <- cgen expr
--    iadd x constantOne
--  S.Pred expr -> do
--    x <- cgen expr
--    isub x constantOne
--  S.If ifexpr tr fl -> do
--    -- make our blocks
--    ifthen <- addBlock $ fromString "if.then"
--    ifelse <- addBlock $ fromString "if.else"
--    ifexit <- addBlock $ fromString "if.exit"
--
--    -- %entry
--    ----------
--    cond <- cgen ifexpr
--    test <- icmp IP.NE constantZero cond
--    -- Branch based on condition
--    void $ cbr test ifthen ifelse
--
--    -- if.then
--    ----------
--    setBlock ifthen
--    -- Generate the true branch code
--    trval <- cgen tr
--    -- Branch to the merge block
--    void $ br ifexit
--    ifthen1 <- getBlock
--
--    -- if.else
--    ----------
--    setBlock ifelse
--    -- Generate the false branch code
--    flval <- cgen fl
--    void $ br ifexit
--    ifelse1 <- getBlock
--
--    -- if.exit
--    ----------
--    setBlock ifexit
--    phi T.double [(trval, ifthen1), (flval, ifelse1)]
--
--codegenTop :: S.Expr -> LLVM ()
--codegenTop expr = define T.void (fromString "main") [] blks
--  -- we currently don't have any top level declarations in mossy
--  where blks = createBlocks $ execCodegen $ do
--          ent <- addBlock entryBlockName
--          _   <- setBlock ent
--          void $ cgen expr
--          retVoid

codegenTop :: S.Expr -> TranslationUnit
codegenTop = undefined
---------------------------------------------------------------------------------
---- Compilation
---------------------------------------------------------------------------------
--codegen :: AST.Module -> [S.Expr] -> IO AST.Module
--codegen mdl fns = runJIT oldast
--  where modn   = mapM codegenTop fns
--        oldast = runLLVM mdl modn
codegen :: S.Expr -> TranslationUnit
codegen = codegenTop
