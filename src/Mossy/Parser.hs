module Mossy.Parser where

import           Data.Functor.Identity (Identity (..))
import           Text.Parsec           (alphaNum, eof, letter, oneOf, parse,
                                        (<|>), many1)
import           Text.Parsec.Error     (ParseError)
import           Text.Parsec.Expr      as Ex
import           Text.Parsec.String    (Parser)
import           Text.Parsec.Token     (GenLanguageDef (LanguageDef),
                                        LanguageDef)
import qualified Text.Parsec.Token     as Tok
import           Text.Parsec.Language    (haskellStyle)

import           Mossy.Syntax

style :: LanguageDef ()
style = haskellStyle { Tok.reservedOpNames = ["->", "\\", "+", "*", "-", "="]
                     , Tok.reservedNames   = ["True", "False"]
                     }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style

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

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

identifier :: Parser String
identifier = Tok.identifier lexer
--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------
natural :: Parser Integer
natural = Tok.natural lexer

variable :: Parser Expr
variable = do
  x <- identifier
  return (Var x)

number :: Parser Expr
number = do
  n <- natural
  return (Lit (LInt (fromIntegral n)))

bool :: Parser Expr
bool = do
  b <- (True <$ reserved "True") <|> (False <$ reserved "False")
  return $ Lit $ LBool b

lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  args <- many1 identifier
  reservedOp "->"
  body <- expr
  return $ foldr Lam body args

lit :: Parser Expr
lit = number <|> bool

term :: Parser Expr
term =  parens expr
    <|> variable
    <|> lit
    <|> lambda

expr :: Parser Expr
expr = do
  es <- many1 term
  return (foldl1 App es)

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"
