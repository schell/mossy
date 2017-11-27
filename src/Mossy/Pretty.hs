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

import qualified Language.GLSL                  as AST
import           Text.PrettyPrint               (Doc, char, hsep, sep, text,
                                                 (<+>), (<>))
import qualified Text.PrettyPrint               as PP
import qualified Text.PrettyPrint               as PP
import qualified "pretty" Text.PrettyPrint.HughesPJClass as PP
import qualified "prettyclass" Text.PrettyPrint.HughesPJClass as PC

class MyPretty p where
  ppr :: Int -> p -> Doc

  pp :: p -> Doc
  pp = ppr 0

instance MyPretty Name where
  ppr _ = text


viewVars :: Expr -> [Name]
viewVars (Lam n a) = n : viewVars a
viewVars _         = []

viewBody :: Expr -> Expr
viewBody (Lam _ a) = viewBody a
viewBody x         = x

viewApp :: Expr -> (Expr, [Expr])
viewApp (App e1 e2) = go e1 [e2]
  where
    go (App a b) xs = go a (b : xs)
    go f xs         = (f, xs)
viewApp _ = error "not application"

parensIf ::  Bool -> Doc -> Doc
parensIf True  = PP.parens
parensIf False = id

instance MyPretty Expr where
  ppr p = \case
    Lit (LInt a)  -> text $ show a
    Lit (LBool b) -> text $ show b
    Var x         -> text x
    e@(App _ _)   -> let (f, xs) = viewApp e
                     in parensIf (p > 0) $ ppr p f <+> sep (ppr (p + 1) <$> xs)
    e@(Lam _ _)   -> let vars = map pp      $ viewVars e
                         body = ppr (p + 1) $ viewBody e
                     in parensIf (p > 0) $ char '\\' <>  hsep vars
                                                     <+> "->"
                                                     <+> body


ppexpr :: Expr -> String
ppexpr = PP.render . ppr 0

pcShow :: PC.Pretty a => a -> String
pcShow = show . PC.pPrint

ppShow :: PP.Pretty a => a -> String
ppShow = PP.prettyShow
