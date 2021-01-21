module Damask.Parser where

import Data.Functor (($>))
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text
import Prelude hiding (break)

type Poem = Expr

data Expr
  = Literal T.Text
  | Break Char
  | Sequence [Expr]
  | FLIP Expr Expr
  | PUSH Expr
  | SHIFT T.Text Expr
  deriving (Show)

poem :: Parser Poem
poem = expr <* eof

-- breaking up expr and term is necessary for left recursion (using chainl1)
expr :: Parser Expr
expr = maybeSequence <$> many1 (break <|> term `chainl1` flipOp)

term :: Parser Expr
term = maybeSequence <$> many1 (pushExpr <|> try shiftExpr <|> literalExpr)

maybeSequence :: [Expr] -> Expr
maybeSequence es = if length es > 1 then Sequence es else head es

pushExpr :: Parser Expr
pushExpr = pushOp <*> term

shiftExpr :: Parser Expr
shiftExpr = do
  lWord <- fmap T.pack $ (<>) <$> many1 letter <*> manyTill (char ' ') (char shiftSym)
  rExpr <-
    maybeSequence <$> try (many1 (try shiftExpr <|> literalExpr))
      <|> try (Literal . T.pack <$> count (T.length lWord) litChar)
      <|> Literal . T.pack <$> many1 litChar
  return $ SHIFT lWord rExpr

flipOp :: Parser (Expr -> Expr -> Expr)
flipOp = optionalSpace flipSym $> FLIP

pushOp :: Parser (Expr -> Expr)
pushOp = optionalSpace pushSym $> PUSH

optionalSpace :: Char -> Parser Char
optionalSpace c = char c <* optional (char ' ')

literalExpr :: Parser Expr
literalExpr = Literal . T.pack <$> many1 litChar

break :: Parser Expr
break = Break <$> (newline <|> char '\t')

litChar :: Parser Char
litChar = letter <|> char ' '

flipSym, pushSym, shiftSym :: Char
flipSym = ','
pushSym = ':'
shiftSym = '—'
