{-# LANGUAGE TypeFamilies        #-}

module Rauzy.Substitution
    where
  
import Rauzy.Alphabet
import Data.List
import Prelude hiding (Word)





type Substitution a = (Word a -> Word a, a)

    
-- Limit Word of substitution
limit :: Alphabet a => Substitution a -> Word a
limit (s,i) = collapse $ iterate s [i]
  where
    collapse (l1:l2:ls) = if all id $ zipWith (==) l1 l2
                          then l1 ++ (collapse $ map (drop (length l1)) (l2:ls))
                          else collapse (l2:ls)

    collapse (l1:[])  = l1
    collapse [] = []



primitive :: Alphabet a => Substitution a -> Bool
primitive (s,i) = all transitive (alphabet i)
    where
      transitive c = primitive' $ map (countZeros . abelianize) $ tail $ iterate s [c]
      countZeros l = length $ filter (\x -> snd x == 0) l
      primitive' (0:xs) = True
      primitive' (x1:x2:xs) = if   x1 == x2
                              then False     -- implicitly x1 /= 0
                              else primitive' (x2:xs)

abelianize :: Alphabet a => Word a -> [(a, Int)]
abelianize w = zip alph $ foldl count initial w
    where
      alph    = alphabet' `asTypeOf` w
      initial = [ 0 | _ <- alph ]
      count l c = zipWith (+) [ if k == c then 1 else 0 | k <- alph ] l


-- subToMatrix :: Alphabet a => Substitution a -> [[Int]]
subToMatrix (s,i) = transpose [ map snd $ abelianize (s c) | c <- alphabet i ]

                   
subToFactorList :: Alphabet a => Substitution a -> [[Word a]]
subToFactorList (s,i) = [[]] : (map snd $ iterate subToFactors (1,map (:[]) l1))
    where
      l1 = let w = map nub $ iterate s [i]
           in fst $ head $ dropWhile (\(x,y) -> x /= y) $ zip w (tail w) 
      next n l = nub $ concatMap (factors n) $ map s l
      subToFactors (n,l) = (n+1, tillHalt (n+1) ([], next (n+1) l))
      tillHalt n (l,[])  = l
      tillHalt n (l,r)   = let old = l ++ r
                           in tillHalt n (old, next n r \\ old)

subToEdges :: Alphabet a => Substitution a -> [[(Word a, Word a, a)]]
subToEdges w = (map.map) slide $ tail $ tail $ subToFactorList w
    where
      slide x = (init x, tail x, last x)
                              
complexity :: Alphabet a => [[Word a]] -> [Double]
complexity list = 1 : zipWith (/) (tail $ map (fromIntegral . length) list) (map fromIntegral [1..])

  
