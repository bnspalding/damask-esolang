{-# LANGUAGE OverloadedStrings #-}

module Main where

import Damask.Shift
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs, getEnv)

main :: IO ()
main = do
  startingWord <- T.pack . head <$> getArgs
  dictFile <- getEnv "MYLEX"
  dictWords <- T.lines <$> TIO.readFile dictFile
  if startingWord `notElem` dictWords
    then TIO.putStrLn $ startingWord <> "not present in dictionary"
    else mapM_ TIO.putStrLn $ findValidShifts startingWord dictWords

findValidShifts :: T.Text -> [T.Text] -> [T.Text]
findValidShifts startingWord dictWords =
  formatResult
    <$> filter
      ((`elem` dictWords) . snd)
      ((\w -> (w, applyShift startingWord w)) <$> dictWords)

formatResult :: (T.Text, T.Text) -> T.Text
formatResult (original, transform) = original <> " -> " <> transform
