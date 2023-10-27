module Main (main) where

import Lib
import System.Environment
import Control.Monad

-- lineByLine f = unlines . map f . lines

arrays :: String
arrays = replicate 1000 '[' ++ replicate 1000 ']'

main :: IO ()
main = do
  putStrLn $ output $ deserialize arrays
  -- args <- getArgs
  -- if null args
  --   then interact (output . deserialize)
  --   else mapM_ (readFile >=> (putStrLn . output . deserialize)) args
  where 
    output (Right val) = serialize val
    output (Left e) = show e