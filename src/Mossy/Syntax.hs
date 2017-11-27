module Mossy.Syntax where

type Name = String

data Lit = LInt Int
         | LBool Bool
         deriving (Show, Eq)

data Expr = Var Name
          | App Expr Expr
          | Lam Name Expr
          | Lit Lit
          deriving (Show, Eq)
