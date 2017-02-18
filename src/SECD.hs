{-# LANGUAGE Strict, StrictData #-}

module SECD (runLisp, runLispFile) where

import SECD.Internal
import SECD.Parser
import SECD.Compiler
import SECD.VM

runLisp' :: String -> String -> IO ()
runLisp' s ss = do
  case parse s ss of
    Left err -> print err
    Right ast ->
      case compile ast of
        Left err -> putStrLn err
        Right code -> do
          secd <- runVM $ initVM { code = code }
          print $ head $ stack secd

runLisp :: String -> IO ()
runLisp = runLisp' "runLisp"

runLispFile :: String -> IO ()
runLispFile s = do
  ss <- readFile s
  runLisp' s ss

