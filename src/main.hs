module Main (main) where

import System.Environment(getArgs)
import Interpreter
import Parser
import Optimiser
import Memory

memory = Memory [] 0 []

main = do
  args <- getArgs
  src <- readFile $ head args 
  -- putStr $ show $ optimise $ parse src
  interpreter (optimise $ parse src) memory
  return ()
