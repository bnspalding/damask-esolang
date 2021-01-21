{-# LANGUAGE OverloadedStrings #-}
module Damask.Shift where

import qualified Data.Text as T
import Data.Char (isUpper, toUpper, toLower)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

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
