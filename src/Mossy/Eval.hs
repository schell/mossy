{-# LANGUAGE LambdaCase #-}
module Mossy.Eval where

import           Data.Map             (Map)
import qualified Data.Map             as M

import           Control.Monad.State
import           Control.Monad.Writer

import           Mossy.Syntax

type Step = (Int, String, Expr)

newtype EvalState = EvalState { depth :: Int
                              } deriving (Show)

type Eval a = WriterT [Step] (State EvalState) a

type Scope = Map String Value

emptyScope :: Scope
emptyScope = M.empty

data Value = VInt Int
           | VBool Bool
           | VClosure String Expr Scope
           deriving (Show, Eq)

inc :: Eval a -> Eval a
inc m = do
  modify $ \s -> s{ depth = depth s + 1 }
  out <- m
  modify $ \s -> s{ depth = depth s - 1 }
  return out

red :: String -> Expr -> Eval ()
red str x = do
  d <- gets depth
  tell [(d, str, x)]
  return ()

extend :: Scope -> String -> Value -> Scope
extend env v t = M.insert v t env

apply :: Value -> Value -> Eval Value
apply (VClosure n e clo) ex = eval (extend clo n ex) e
apply _ _ = error "Tried to apply non-closure"

eval :: Scope -> Expr -> Eval Value
eval env expr = case expr of
  Lit (LInt i)  -> return $ VInt i
  Lit (LBool b) -> return $ VBool b
  Var x -> do
    red "variable" expr
    return $ env M.! x
  Lam x _ body -> inc $ do
    red "lamda" body
    return $ VClosure x body env
  App a b    -> inc $ do
    x <- eval env a
    red "apply lhs" a
    y <- eval env b
    red "apply rhs" b
    apply x y

runEval :: Expr -> (Value, [Step])
runEval x = evalState (runWriterT (eval emptyScope x)) (EvalState 0)
