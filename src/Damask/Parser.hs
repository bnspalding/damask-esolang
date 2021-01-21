module Damask.Parser where

import Data.Functor (($>))
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text
import Prelude hiding (Word, break)

type Poem = Expr

data Expr
  = Literal T.Text
  | Break Char
  | Sequence [Expr]
  | FLIP Expr Expr
  | PUSH Expr
  | SHIFT Word Word
  deriving (Show)

newtype Word = Word T.Text deriving (Show)

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
  lWord <- (:) <$> letter <*> manyTill litChar (char shiftSym)
  rWord <-
    try (count (length lWord) litChar)
      <|> many1 litChar
  return $ SHIFT (Word (T.pack lWord)) (Word (T.pack rWord))

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
