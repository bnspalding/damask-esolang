{-# LANGUAGE OverloadedStrings #-}

module Damask
  ( runPoem,
  )
where

import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text

runPoem :: T.Text -> T.Text
runPoem = either showParseError runAST . parse poem "(source poem)"

-- each stanza of a source-poem is independent
type Poem = [Stanza]

-- a stanza is one or more expressions appended together
type Stanza = [Expr]

data Expr
  = Terminal T.Text
  | Operation Operator

data Operator
  = FLIP Expr Expr
  | PUSH Expr
  | SHIFT Expr Expr
  | FIRST Expr

runAST :: Poem -> T.Text
runAST = T.intercalate "\n" . fmap evalStanza

evalStanza :: Stanza -> T.Text
evalStanza = undefined

evalExpr :: Expr -> T.Text
evalExpr = undefined

showParseError :: ParseError -> T.Text
showParseError = T.pack . show

poem :: Parser Poem
poem = sepBy1 stanza (newline *> newline)

stanza :: Parser Stanza
stanza = many expr

operatorSymbols :: [Parser Char]
operatorSymbols = [symFlip, symPush, symShift, symFirst]

symFlip :: Parser Char
symFlip = char ','

symPush :: Parser Char
symPush = char ':'

symShift :: Parser Char
symShift = char '—'

symFirst :: Parser Char
symFirst = char '\t'

exprFlip :: Parser Expr
exprFlip = Operation <$> (FLIP <$> lineLeft <* symFlip <*> lineRight)

exprPush :: Parser Expr
exprPush = Operation . PUSH <$> (symPush *> lineRight)

exprShift :: Parser Expr
exprShift = Operation <$> (SHIFT <$> wordLeft <* symShift <*> wordRight)

exprFirst :: Parser Expr
exprFirst = Operation . FIRST <$> (symFirst *> lineRight)

lineLeft :: Parser Expr
lineLeft = breakpoint *> expr

lineRight :: Parser Expr
lineRight = expr <* newline

wordLeft :: Parser Expr
wordLeft = Terminal . T.pack <$> (space *> many anyChar)

wordRight :: Parser Expr
wordRight = Terminal . T.pack <$> manyTill anyChar space

expr :: Parser Expr
expr = try operation <|> terminal

operation :: Parser Expr
operation = try exprFlip <|> try exprPush <|> try exprShift <|> try exprFirst

terminal :: Parser Expr
terminal = Terminal . T.pack <$> manyTill anyChar operation

breakpoint :: Parser Char
breakpoint = newline <|> choice operatorSymbols
