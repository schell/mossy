module Mossy.Parser where

import           Data.Functor.Identity (Identity (..))
import           Text.Parsec           (alphaNum, eof, letter, oneOf, parse,
                                        (<|>))
import           Text.Parsec.Error     (ParseError)
import           Text.Parsec.Expr      as Ex
import           Text.Parsec.String    (Parser)
import           Text.Parsec.Token     (GenLanguageDef (LanguageDef),
                                        LanguageDef)
import qualified Text.Parsec.Token     as Tok

import           Mossy.Syntax

langDef :: LanguageDef ()
langDef = LanguageDef { Tok.commentStart    = "{-"
                      , Tok.commentEnd      = "-}"
                      , Tok.commentLine     = "--"
                      , Tok.nestedComments  = True
                      , Tok.identStart      = letter
                      , Tok.identLetter     = alphaNum <|> oneOf "_'"
                      , Tok.opStart         = oneOf opChars
                      , Tok.opLetter        = oneOf opChars
                      , Tok.reservedNames   = reservedNames
                      , Tok.reservedOpNames = reservedOps
                      , Tok.caseSensitive   = True
                      }
  where opChars = ":!#$%&*+./<=>?@\\^|-~"
        reservedNames = []
        reservedOps   = []

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

parens :: Parser a -> Parser a
parens = Tok.parens lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

prefixOp :: String -> (a -> a) -> Ex.Operator String () Identity a
prefixOp s f = Ex.Prefix (reservedOp s >> return f)

--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------
-- | Blessed prefix operators.
table :: Ex.OperatorTable String () Identity Expr
table = [[ prefixOp "succ" Succ
         , prefixOp "pred" Pred
         , prefixOp "iszero" IsZero
         ]
        ]

-- | if expr then expr else expr
ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- expr
  reservedOp "then"
  tr <- expr
  reserved "else"
  fl <- expr
  return $ If cond tr fl

-- | constants
true, false, zero :: Parser Expr
true  = reserved "true"  >> return Tr
false = reserved "false" >> return Fl
zero  = reserved "0"     >> return Zero

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

factor :: Parser Expr
factor = true <|> false <|> zero <|> ifthen <|> parens expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"
