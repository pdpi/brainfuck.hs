module Parser where

import Brainfuck

type Parser = [Program]

push :: Parser -> Parser
push p = []:p

pop :: Parser -> Parser
pop (p:p':ps) = ((Loop $ reverse p):p'):ps

put :: Instruction -> Parser -> Parser
put i (p:ps) = (i:p):ps

parse :: String -> Program
parse src = reverse . head . fst $ parsePartial ([[]], src)

parsePartial :: (Parser, String) -> (Parser, String)
parsePartial (p, ('.':cs)) = parsePartial (put Put  p, cs)
parsePartial (p, (',':cs)) = parsePartial (put Get  p, cs)
parsePartial (p, ('>':cs)) = parsePartial (put Next p, cs)
parsePartial (p, ('<':cs)) = parsePartial (put Prev p, cs)
parsePartial (p, ('+':cs)) = parsePartial (put Inc  p, cs)
parsePartial (p, ('-':cs)) = parsePartial (put Dec  p, cs)
parsePartial (p, ('[':cs)) = parsePartial (push p,     cs)
parsePartial (p, (']':cs)) = parsePartial (pop p,      cs)
parsePartial (p, (_  :cs)) = parsePartial (p,          cs)
parsePartial (p, [])       = (p, [])