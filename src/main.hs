module Main (main) where

import System.Environment(getArgs)
import Brainfuck
import Parser

memory = Memory [] 0 []

main = do
  args <- getArgs
  src <- readFile $ head args 
  bf (parse src) memory
  return ()