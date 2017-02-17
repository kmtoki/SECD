module Main where

import SECD
import System.Environment
import System.Directory

main = do
  args <- getArgs
  case args of
    [file] -> do
      isExist <- doesFileExist file
      if isExist then
        runLispFile file
      else
        putStrLn $ file ++ " not found"
    _ -> putStrLn $ "usage: lisp <file>"

