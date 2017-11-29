{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Mossy
    ( repl
    , module M
    ) where

import           Control.Monad            (when, mapM_)
import           Control.Monad.IO.Class   (liftIO)
import           Data.Function            (fix)
import           Language.GLSL            ()
import           System.Console.Haskeline (defaultSettings, getInputLine,
                                           outputStrLn, runInputT)
import           Mossy.Emit               as M
import           Mossy.Eval               as M
import           Mossy.Parser
import           Mossy.Pretty             as M
import           Mossy.Syntax             as M
import           Mossy.TypeCheck          as M

newtype Options = Options { optsEmitIR :: Bool }

showStep :: (Int, String, Expr) -> IO ()
showStep (d, str, x) = putStrLn $ unwords [replicate d ' ', "=>", str, ppexpr x]

process :: Options -> String -> IO ()
process opts line = do
  let res = parseExpr line
  case res of
    Left perr -> print perr
    Right ex -> case check [] ex of
      Left tyerr -> print tyerr
      Right _ -> do
        let (out, steps) = runEval ex
        mapM_ showStep steps
        putStrLn $ replicate 80 '='
        print out
        when (optsEmitIR opts) $ do
          putStrLn $ replicate 80 '-'
          unit <- codegen ex
          putStrLn $ pcShow unit
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
