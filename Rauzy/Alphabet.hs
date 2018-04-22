{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rauzy.Alphabet
    where  

import Data.Char (toLower)
import Data.List
import Prelude hiding (Word)



{--- Alphabets and Words ---}
class (Read a, Show a, Enum a, Eq a, Ord a) => Alphabet a where
  alphabet  :: a -> [a]
  alphabet' :: [a]

type Word a = [a]

instance Alphabet a => Enum [a] where
  toEnum 0 = []
  toEnum n = reverse $ toEnum' n
    where
      toEnum' (-1)  = []
      toEnum'    0  = []
      toEnum'    m  = if   r == 0
                      then rot' (toEnum r) : (toEnum' (q-1))
                      else rot' (toEnum r) : (toEnum' q)
        where
          (q,r) = m `quotRem` base

      base  = length (alphabet' :: [a])


  fromEnum l = sum $ zipWith (*)
               [ 1 + (fromEnum a) | a <- reverse l ]
               [ base^n | n <- [0 .. ] ]
    where
      base  = length (alphabet' :: [a])


rot :: Alphabet a => a -> a
rot c = (toEnum . inc . fromEnum) c
  where
    inc n = (n+1) `mod` (length (alphabet c))


rot' :: Alphabet a => a -> a
rot' c = (toEnum . dec . fromEnum) c
  where
    dec n = (n-1) `mod` (length $ alphabet c)


toStr :: Alphabet a => Word a -> String
toStr w = map toLower $ concatMap show w

-- Finite Words Only!
factors :: Alphabet a => Int -> Word a -> [Word a]
factors 0 _ = [[]]
factors n w = sort $ nub $ orderedFactors n w

orderedFactors :: Alphabet a => Int -> Word a -> [Word a]
orderedFactors n w = if   length w < n
                     then []
                     else take n w : (orderedFactors n $ tail w)
{--- end of Alphabets and Words ---}






{--- Alphabets ---}
  
data Binary = A2 | B2
   deriving (Read, Enum, Eq, Ord)

instance Show Binary where
    show A2 = "a"
    show B2 = "b"
   
instance Alphabet Binary where
    alphabet _ = [A2,B2]
    alphabet'  = [A2,B2]

--------------------------------------------------
                 
data Ternary = A3 | B3 | C3
   deriving (Read, Enum, Eq, Ord)

instance Show Ternary where
    show A3 = "a"
    show B3 = "b"
    show C3 = "c"
            
instance Alphabet Ternary where
  alphabet _ = [A3,B3,C3]
  alphabet'  = [A3,B3,C3]

--------------------------------------------------
                 
data Quaternary = A4 | B4 | C4 | D4
   deriving (Read, Enum, Eq, Ord)

instance Show Quaternary where
    show A4 = "a"
    show B4 = "b"
    show C4 = "c"
    show D4 = "d"
            
instance Alphabet Quaternary where
  alphabet _ = [A4,B4,C4,D4]
  alphabet'  = [A4,B4,C4,D4]

--------------------------------------------------
                 
data Quinary = A5 | B5 | C5 | D5 | E5
   deriving (Read, Enum, Eq, Ord)

instance Show Quinary where
    show A5 = "a"
    show B5 = "b"
    show C5 = "c"
    show D5 = "d"
    show E5 = "e"
            
instance Alphabet Quinary where
  alphabet _ = [A5,B5,C5,D5,E5]
  alphabet'  = [A5,B5,C5,D5,E5]

--------------------------------------------------
                 
data Senary = A6 | B6 | C6 | D6 | E6 | F6
   deriving (Read, Enum, Eq, Ord)

instance Show Senary where
    show A6 = "a"
    show B6 = "b"
    show C6 = "c"
    show D6 = "d"
    show E6 = "e"
    show F6 = "f"
            
instance Alphabet Senary where
  alphabet _ = [A6,B6,C6,D6,E6,F6]
  alphabet'  = [A6,B6,C6,D6,E6,F6]

--------------------------------------------------

data Septenary = A7 | B7 | C7 | D7 | E7 | F7 | G7
   deriving (Read, Enum, Eq, Ord)

instance Show Septenary where
    show A7 = "a"
    show B7 = "b"
    show C7 = "c"
    show D7 = "d"
    show E7 = "e"
    show F7 = "f"
    show G7 = "g"
            
instance Alphabet Septenary where
  alphabet _ = [A7,B7,C7,D7,E7,F7,G7]
  alphabet'  = [A7,B7,C7,D7,E7,F7,G7]

--------------------------------------------------

data Octal = A8 | B8 | C8 | D8 | E8 | F8 | G8 | H8
   deriving (Read, Enum, Eq, Ord)

instance Show Octal where
    show A8 = "a"
    show B8 = "b"
    show C8 = "c"
    show D8 = "d"
    show E8 = "e"
    show F8 = "f"
    show G8 = "g"
    show H8 = "h"
            
instance Alphabet Octal where
  alphabet _ = [A8,B8,C8,D8,E8,F8,G8,H8]
  alphabet'  = [A8,B8,C8,D8,E8,F8,G8,H8]

{--- end of Alphabets ---}

              
-- Nonary,     Decimal,      Undecimal,    Duodecimal
-- Tridecimal, Tetradecimal, Pentadecimal, Hexadecimal


