{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Mossy
    ( repl
    , module M
    ) where

import           Control.Monad            (when)
import           Control.Monad.IO.Class   (liftIO)
import           Data.Function            (fix)
import           Data.Maybe               (fromMaybe)
import qualified LLVM.AST                 as AST
import           System.Console.Haskeline (defaultSettings, getInputLine,
                                           outputStrLn, runInputT)
--import           Mossy.Codegen            as M
import           Mossy.Emit               as M
import           Mossy.Eval               as M
import           Mossy.Parser             as M
import           Mossy.Pretty             as M
import           Mossy.Syntax             as M

newtype Options = Options { optsEmitIR :: Bool }

showStep :: (Int, Expr) -> IO ()
showStep (d, x) = putStrLn $ replicate d ' ' ++ "=> " ++ ppexpr x

process :: Options -> String -> IO ()
process opts line = do
  let res = parseExpr line
  case res of
    Left err -> print err
    Right ex -> do
      let (out, steps) = runEval ex
      mapM_ showStep steps
      print out
      putStrLn $ replicate 80 '-'
      when (optsEmitIR opts) $ do
        putStrLn $ ppGLSL $ codegen ex
        putStrLn $ replicate 80 '-'

initOpts :: Options
initOpts = Options False

repl :: IO ()
repl = runInputT defaultSettings $
  ($ initOpts) $ fix $ \loop opts ->
    getInputLine "λ " >>= \case
      Nothing    -> outputStrLn "bye!"
      Just ":q"  -> outputStrLn "bye!"
      Just ":set +ir" -> do
        liftIO $ putStrLn "Emitting generated code."
        loop opts{ optsEmitIR = True }
      Just ":set -ir" -> do
        liftIO $ putStrLn "Not emitting generated code."
        loop opts{ optsEmitIR = False }
      Just "" -> loop opts
      Just input -> do
        liftIO $ process opts input
        loop opts
