module Hw4 where

import Control.Applicative
import Data.Char
import Parser
import Hw3

var :: Parser Expr
var = Var <$> some alphaNum

lambda :: Parser Expr
lambda = do
  string "(\\"
  symbol <- some alphaNum
  char '.'
  body <- expr
  char ')'
  return (Lambda symbol body)

app :: Parser Expr
app = do
  char '('
  expr1 <- expr
  sep
  expr2 <- expr
  char ')'
  return (App expr1 expr2)

expr :: Parser Expr
expr = var <|> app <|> lambda

definition :: Parser (Symbol, Expr)
definition = do
  name <- some alphaNum
  sep
  string ":="
  sep
  body <- expr
  return (name, body)

prg :: Parser Expr
prg = do
  definitions <- many (definition <* sep)
  resolve definitions <$> expr

resolve :: [(Symbol, Expr)] -> Expr -> Expr
resolve definitions expr = case expr of
  Var name -> case lookup name definitions of
    Just e -> resolve definitions e
    Nothing -> expr
  App e1 e2 -> App (resolve definitions e1) (resolve definitions e2)
  Lambda name body -> Lambda name (resolve definitions body)

readPrg :: String -> Maybe Expr
readPrg input =
  case parse prg input of
    Just (expr, _) -> if length input == 1 then Just (Var input)
                      else if length (show expr) == 1 then Nothing else Just expr
    _ -> Nothing
