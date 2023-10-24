module Main (main) where

import Lib
import System.Environment
import Control.Monad

-- lineByLine f = unlines . map f . lines

main :: IO ()
main = do
  args <- getArgs
  if null args
    then interact (show . deserialize)
    else mapM_ (readFile >=> (print . deserialize)) args