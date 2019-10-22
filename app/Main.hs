module Main where

import Lib
import Code
import BWMachine
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let filename = args !! 0
  codes <- Code.readFile filename
  let pure_codes = Code.rmComment codes
  Prelude.putStrLn $ show pure_codes
  BWMachine.runProgram pure_codes
