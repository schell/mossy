module Mossy.Syntax
  ( module Mossy.Syntax
  , module Type
  ) where

import Mossy.Type as Type

data Ground = LInt Int
            | LBool Bool
            deriving (Show, Eq)

data Expr = Var String
          | Lit Ground
          | App Expr Expr
          | Lam String Type Expr
          deriving (Show, Eq)
