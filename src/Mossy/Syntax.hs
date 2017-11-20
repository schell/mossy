module Mossy.Syntax where

data Expr = Tr
          | Fl
          | Zero
          | IsZero Expr
          | Succ Expr
          | Pred Expr
          | If Expr Expr Expr
          deriving (Show, Eq)
