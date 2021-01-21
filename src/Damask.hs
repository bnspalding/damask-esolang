{-# LANGUAGE OverloadedStrings #-}

module Damask
  ( runPoem,
  )
where

import Data.Char
import Data.Functor (($>))
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text
import Prelude hiding (Word, break)

runPoem :: T.Text -> T.Text
runPoem = either showParseError evalPoem . parse poem "(source poem)"

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

showParseError :: ParseError -> T.Text
showParseError = T.pack . show

evalPoem :: Poem -> T.Text
evalPoem = getResult . evalExpr

data Result = Result T.Text T.Text

instance Semigroup Result where
  (<>) (Result w1 p1) (Result w2 p2) = Result (w1 <> w2) (p1 <> p2)

getResult :: Result -> T.Text
getResult (Result w p) = w <> p

write :: T.Text -> Result
write t = Result t ""

push :: T.Text -> Result
push t = Result "" ("\n" <> T.stripStart t)

evalExpr :: Expr -> Result
evalExpr e = case e of
  FLIP l r -> evalExpr r <> write ", " <> evalExpr l
  PUSH r -> push $ getResult $ evalExpr r
  SHIFT l r -> write $ applyShift l r
  Sequence es -> foldl1 (<>) $ fmap evalExpr es
  Break c -> write $ T.singleton c
  Literal t -> write t

applyShift :: Word -> Word -> T.Text
applyShift (Word l) (Word r) = T.zipWith shift l' r'
  where
    l' = if difflr < 0 then l <> T.replicate (abs difflr) " " else l
    r' = if difflr > 0 then r <> T.replicate difflr " " else r
    difflr = T.length l - T.length r

shift :: Char -> Char -> Char
shift c1 c2 = maybe c1 (maybeCapitalize . shiftI) c1index
  where
    c1normal = toLower c1
    c2normal = toLower c2
    c1index = elemIndex c1normal charList
    c2index = fromMaybe 0 (elemIndex c2normal charList)
    shiftI = \start -> charList !! ((start + c2index) `mod` length charList)
    maybeCapitalize = if isUpper c1 then toUpper else id

charList :: String
charList = " abcdefghijklmnopqrstuvwxyz"

-- PARSING --------------------------------------------

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
