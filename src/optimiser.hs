module Optimiser where

import Interpreter

optimise :: Program -> Program
optimise (Inc:Inc     :is) = optimise ((Add 2):is)
optimise ((Add n):Inc :is) = optimise ((Add $ n+1):is)
optimise (Dec:Dec     :is) = optimise ((Sub 2):is)
optimise ((Sub n):Dec :is) = optimise ((Sub $ n+1):is)
optimise (Inc:Dec     :is) = optimise is
optimise ((Add 1):Dec :is) = optimise is
optimise (Dec:Inc     :is) = optimise is
optimise ((Sub 1):Inc :is) = optimise is
optimise ((Add n):Dec :is) = optimise ((Add $ n-1):is)
optimise ((Sub n):Inc :is) = optimise ((Sub $ n-1):is)
optimise ((Loop s)    :is) = (Loop $ optimise s):(optimise is)
optimise (i           :is) = i:optimise is
optimise []                = []