{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Mossy
    ( repl
    , module M
    ) where

import           Control.Monad.IO.Class   (liftIO)
import           Data.Function            (fix)
import           Data.Maybe               (fromMaybe)
import           System.Console.Haskeline (defaultSettings, getInputLine,
                                           outputStrLn, runInputT)

import           Mossy.Parser             as M
import           Mossy.Pretty             as M
import           Mossy.Syntax             as M

isNum :: Expr -> Bool
isNum = \case Zero     -> True
              (Succ t) -> isNum t
              _        -> False

isVal :: Expr -> Bool
isVal = \case Tr -> True
              Fl -> True
              t  -> isNum t

smallStep :: Expr -> Maybe Expr
smallStep = \case
  IsZero Zero               -> Just Tr
  IsZero (Succ t) | isNum t -> Just Fl
  IsZero t                  -> IsZero <$> smallStep t
  Succ t                    -> Succ   <$> smallStep t
  Pred Zero                 -> Just Zero
  Pred (Succ t) | isNum t   -> Just t
  Pred t                    -> Pred   <$> smallStep t
  If Tr c _                 -> Just c
  If Fl _ a                 -> Just a
  If t  c a                 -> If <$> smallStep t <*> pure c <*> pure a
  _                         -> Nothing

nf :: Expr -> Expr
nf x = fromMaybe x (nf <$> smallStep x)

-- | Big step
eval :: Expr -> Maybe Expr
eval t
  | nft <- nf t
  , isVal nft = Just nft
  | otherwise = Nothing -- term is "stuck"

process :: String -> IO ()
process line = do
  let res = parseExpr line
  case res of
    Left err -> print err
    Right ex -> case eval ex of
      Nothing     -> putStrLn "Cannot evaluate"
      Just result -> putStrLn $ ppexpr result

repl :: IO ()
repl = runInputT defaultSettings $ fix $ \loop ->
  getInputLine "mossy> " >>= \case
    Nothing    -> outputStrLn "bye!"
    Just input -> liftIO (process input) >> loop
