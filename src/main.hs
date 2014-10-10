module Main (main) where

import System.Environment(getArgs)
import Interpreter
import Parser
import Memory

memory = Memory [] 0 []

main = do
  args <- getArgs
  src <- readFile $ head args 
  interpreter (parse src) memory
  return ()
