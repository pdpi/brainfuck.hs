module Parser where

import Brainfuck

type Parser = [Program]

push :: Parser -> Parser
push p = []:p

pop :: Parser -> Parser
pop (p:p':ps) = ((Loop $ reverse p):p'):ps

add :: Parser -> Instruction -> Parser
add (p:ps) i = (i:p):ps

parse :: String -> Program
parse src = reverse . head $ foldl parseChar [[]] src

parseChar :: Parser -> Char -> Parser
parseChar p '.' = add p Put
parseChar p ',' = add p Get
parseChar p '>' = add p Next
parseChar p '<' = add p Prev
parseChar p '+' = add p Inc
parseChar p '-' = add p Dec
parseChar p '[' = push p
parseChar p ']' = pop p
parseChar p _ = p