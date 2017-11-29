{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PackageImports       #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Mossy.Pretty
  ( ppexpr
  , ppShow
  , pcShow
  , PP.render
  ) where

import           Mossy.Syntax

import           Text.PrettyPrint               (Doc, char, parens, text, (<+>))
import qualified Text.PrettyPrint               as PP
import qualified "pretty" Text.PrettyPrint.HughesPJClass as PP
import qualified "prettyclass" Text.PrettyPrint.HughesPJClass as PC

class MyPretty p where
  ppr :: Int -> p -> Doc

  pp :: p -> Doc
  pp = ppr 0

instance MyPretty String where
  ppr _ = text

parensIf ::  Bool -> Doc -> Doc
parensIf True  = PP.parens
parensIf False = id

instance MyPretty Type where
  ppr _ TBool = text "Bool"
  ppr _ TInt  = text "Nat"
  ppr p (TArr a b) = parensIf (isArrow a) (ppr p a) <+> text "->" <+> ppr p b
    where isArrow TArr{} = True
          isArrow _      = False

instance MyPretty Expr where
  ppr p = \case
    Var x -> text x
    Lit (LInt a) -> text (show a)
    Lit (LBool b) -> text (show b)
    App a b -> parensIf (p>0) (ppr (p+1) a) <+> ppr p b
    Lam x t a -> parensIf (p > 0) $ char '\\'
                   <+> parens (text x <+> char ':' <+> ppr p t)
                   <+> text "->"
                   <+> ppr (p+1) a

ppexpr :: MyPretty a => a -> String
ppexpr = PP.render . ppr 0

pcShow :: PC.Pretty a => a -> String
pcShow = show . PC.pPrint

ppShow :: PP.Pretty a => a -> String
ppShow = PP.prettyShow
