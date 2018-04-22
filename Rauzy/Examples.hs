module Rauzy.Examples
    where

import Rauzy.Alphabet
import Rauzy.Substitution




{--- Thue-Morse word ---}
thueMorseSystem :: Substitution Binary
thueMorseSystem = (s, A2)
    where
      s' A2 = [A2,B2]
      s' B2 = [B2,A2]
      s = concatMap s'
{--- end of Thue-Morse word ---}




{--- Fibonacci word ---}
fibonacciSystem :: Substitution Binary
fibonacciSystem = (s, A2)
    where
      s' A2 = [A2,B2]
      s' B2 = [A2]
      s = concatMap s'
{--- end of Fibonacci word ---}




{--- Tribonacci word ---}
tribonacciSystem :: Substitution Ternary
tribonacciSystem = (s, A3)
    where
      s' A3 = [A3,B3]
      s' B3 = [A3,C3]
      s' C3 = [A3]
      s = concatMap s'
{--- end of Tribonacci word ---}




{--- non-primitive Chacon word ---}
npChaconSystem :: Substitution Binary
npChaconSystem = (s, A2)
    where
      s' A2 = [A2,A2,B2,A2]
      s' B2 = [B2]
      s = concatMap s'
{--- end of non-primitive Chacon word ---}
