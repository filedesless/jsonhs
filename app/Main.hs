module Main (main) where

import Lib
import System.Environment
import Control.Monad

-- lineByLine f = unlines . map f . lines

main :: IO ()
main = do
  args <- getArgs
  if null args
    then interact (output . deserialize)
    else mapM_ (readFile >=> (putStrLn . output . deserialize)) args
  where 
    output (Right val) = serialize val
    output (Left e) = show e