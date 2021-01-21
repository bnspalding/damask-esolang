{-# LANGUAGE OverloadedStrings #-}

module Damask
  ( runPoem,
  )
where

import Damask.Parser
import Data.Char (isUpper, toLower, toUpper)
import Data.Coerce (coerce)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Text.Parsec (ParseError, parse)

-- apply transformations encoded in a source-poem
runPoem :: T.Text -> T.Text
runPoem = either showParseError evalPoem . parse poem "(source poem)"

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
  SHIFT l r -> write $ applyShift (coerce l) (coerce r)
  Sequence es -> foldl1 (<>) $ fmap evalExpr es
  Break c -> write $ T.singleton c
  Literal t -> write t

applyShift :: T.Text -> T.Text -> T.Text
applyShift l r = T.zipWith shift l' r'
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
