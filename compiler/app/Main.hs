module Main where

import qualified Data.Text.IO as T
import Lib (parse)

main :: IO ()
main = do
  input <- T.getContents
  parse input
