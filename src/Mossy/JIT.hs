{-# LANGUAGE LambdaCase #-}
module Mossy.JIT where

import           Control.Monad.Except
import qualified Data.ByteString.Char8 as B
import           Data.String           (fromString)
--import           Foreign.Ptr           (FunPtr, castFunPtr)
--import qualified LLVM.AST              as AST
--import           LLVM.Context
--import qualified LLVM.ExecutionEngine  as EE
--import           LLVM.Module           (moduleAST, moduleLLVMAssembly,
--                                        withModuleFromAST)
--import           LLVM.PassManager      (PassSetSpec (..),
--                                        defaultCuratedPassSetSpec,
--                                        runPassManager, withPassManager)


--foreign import ccall "dynamic" haskFun :: FunPtr (IO ()) -> IO ()
--
--run :: FunPtr a -> IO ()
--run fn = haskFun (castFunPtr fn :: FunPtr (IO ()))
--
--passes :: PassSetSpec
--passes = defaultCuratedPassSetSpec { optLevel = Just 3 }
--
--jit :: Context -> (EE.MCJIT -> IO a) -> IO a
--jit c = EE.withMCJIT c optlevel model ptrelim fastins
--  where
--    optlevel = Just 0  -- optimization level
--    model    = Nothing -- code model ( Default )
--    ptrelim  = Nothing -- frame pointer elimination
--    fastins  = Nothing -- fast instruction selection
--
--runJIT :: AST.Module -> IO AST.Module
--runJIT mdl = withContext $ \context ->
--  jit context $ \executionEngine ->
--    withModuleFromAST context mdl $ \m ->
--      withPassManager passes $ \pm -> do
--        -- Optimization Pass
--        void $ runPassManager pm m
--        optmod <- moduleAST m
--        s      <- moduleLLVMAssembly m
--        B.putStrLn s
--
--        EE.withModuleInEngine executionEngine m $ \ee -> do
--          mainfn <- EE.getFunction ee (AST.Name $ fromString "main")
--          case mainfn of
--            Just fn -> do
--              res <- run fn
--              putStrLn $ "Evaluated to: " ++ show res
--            Nothing -> return ()
--
--        -- Return the optimized module
--        return optmod
