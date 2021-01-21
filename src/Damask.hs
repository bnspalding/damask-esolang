{-# LANGUAGE OverloadedStrings #-}

module Damask
  ( runPoem,
  )
where

import Damask.Parser
import Damask.Shift
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
  SHIFT l r -> write $ applyShift l (getResult (evalExpr r))
  Sequence es -> foldl1 (<>) $ fmap evalExpr es
  Break c -> write $ T.singleton c
  Literal t -> write t
