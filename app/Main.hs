module Main where

import Damask (runPoem)
import qualified Data.Text.IO as TIO

main :: IO ()
main = TIO.getContents >>= TIO.putStr . runPoem
