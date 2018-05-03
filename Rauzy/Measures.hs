module Rauzy.Measures
    where

import Prelude hiding (Word)
import Rauzy.Alphabet
import Data.List


data Tree a = Leaf Rational | Branch (a -> Tree a)

instance (Alphabet a) => Show (Tree a) where
    show (Leaf r) = "Leaf: " ++ (show r)
    show (Branch f) = show [ f a | a <- alphabet' ]

factorToTree :: Alphabet a => [Word a] -> Tree a
factorToTree = factorTree (Leaf 1)
            
factorTree :: Alphabet a => Tree a -> [Word a] -> Tree a
factorTree    t     [] = t
factorTree (Leaf r) l = Branch (\b -> let (w, list) = f b l
                                      in factorTree (Leaf (r * w)) list) 
    where
      f c l'  = let list = map tail $ filter (isPrefixOf [c]) l'
                in ((toRational $ length list) / (toRational $ length l'), filter (not . null) list)


factorsToMeasure :: Alphabet a => [Word a] -> Word a -> Rational
factorsToMeasure l x = find (factorToTree l) x
    where
      find (Leaf r)    []    = r
      find (Branch f)  []    = sum [ find (f b) [] | b <- alphabet' `asTypeOf` x ]
      find (Branch f) (y:ys) = find (f y) ys
