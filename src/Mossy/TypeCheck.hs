{-# LANGUAGE LambdaCase #-}
module Mossy.TypeCheck
  ( check
  ) where

import           Control.Monad.Except
import           Control.Monad.Reader

import           Mossy.Syntax

data TypeError = TypeMismatch Type Type
               | TypeNotFunction Type
               | TypeNotInScope String
               deriving (Show, Eq)

type Env = [(String, Type)]

type Check = ExceptT TypeError (Reader Env)

type Name = String

extend :: (Name, Type) -> Env -> Env
extend = (:)

inEnv :: (Name, Type) -> Check a -> Check a
inEnv = local . extend

lookupVar :: Name -> Check Type
lookupVar x = asks (lookup x) >>= \case
  Just e  -> return e
  Nothing -> throwError $ TypeNotInScope x

typeOf :: Expr -> Check Type
typeOf = \case
  Lit LInt {} -> return TInt
  Lit LBool{} -> return TBool
  Var x         -> lookupVar x
  App e1 e2     -> do
    t2 <- typeOf e2
    typeOf e1 >>= \case
      (TArr a b) | a == t2   -> return b
                 | otherwise -> throwError $ TypeMismatch t2 a
      ty -> throwError $ TypeNotFunction ty
  Lam x t e -> do
    rhs <- inEnv (x,t) (typeOf e)
    return (TArr t rhs)

check :: Env -> Expr -> Either TypeError Type
check env x = flip runReader env $ runExceptT $ typeOf x
