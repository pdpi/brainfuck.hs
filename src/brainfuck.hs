module Brainfuck where

import Prelude hiding (read)
import Control.Monad
import Data.Char

data Instruction = Get
                 | Put
                 | Next
                 | Prev
                 | Inc
                 | Dec
                 | Loop [Instruction] deriving (Show, Eq)

type Program = [Instruction]

data Memory = Memory [Int] Int [Int] deriving (Show)

read :: Memory -> Int
read (Memory _ c _) = c

write :: Memory -> Int -> Memory
write (Memory b _ f) i = Memory b i f

next :: Memory -> Memory
next (Memory b c [])     = Memory (c:b) 0 []
next (Memory b c (f:fs)) = Memory (c:b) f fs

prev :: Memory -> Memory
prev (Memory []     c f) = Memory [] 0 (c:f)
prev (Memory (b:bs) c f) = Memory bs b (c:f)

inc :: Memory -> Memory
inc (Memory b c f) = Memory b (c + 1) f

dec :: Memory -> Memory
dec (Memory b c f) = Memory b (c - 1) f

bf :: Program -> Memory -> IO Memory
bf prog memory = foldM process memory prog

process :: Memory -> Instruction -> IO Memory
process memory Put      = do
  putStr [chr . read $ memory]
  return memory
process memory Get      = do
  char:chars <- getLine
  return (write memory $ ord char)
process memory Next     = return . next $ memory
process memory Prev     = return . prev $ memory
process memory Inc      = return . inc  $ memory 
process memory Dec      = return . dec  $ memory 
process memory (Loop p) = if (read memory) /= 0
  then do
    mem <- bf p memory
    process mem (Loop p)
  else return memory