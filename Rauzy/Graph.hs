module Rauzy.Graph
    where

import Data.List
import Data.Maybe
    
type Vertex = ([Int], Int, [Int])
type Graph  = [Vertex]



edgesToGraph :: [(Int,Int)] -> Graph
edgesToGraph edges = simplifyLabels $ map toV vertices
    where
      vertices = let (l,r) = unzip edges
                 in nub $ l ++ r
      lefts  x = [ l | (l,y) <- edges, x == y ]
      rights x = [ r | (y,r) <- edges, x == y ]
      toV v = (lefts v, v, rights v)


              
              
simplifyLabels :: Graph -> Graph
simplifyLabels g = sortBy vertexOrd $ map toV g
    where
      labels      = getLabels g
      reduce n    = fromJust $ elemIndex n labels
      toV (l,v,r) = (sort $ map reduce l, reduce v, sort $ map reduce r)


                    
                    
                    
special ([l],_,[r]) = False
special     _       = True


graphReduction :: Graph -> Graph
graphReduction = simplifyLabels . graphReduction


graphReduction' :: Graph -> Graph
graphReduction' g = case find removable g of
                     Nothing -> g
                     Just v  -> graphReduction' $ joinOver g v
    where                      
      removable ([l],v,[r]) = l /= r   -- don't remove loops
      removable _ = False

joinOver g x@([l],v,[r]) = concatMap specialMap g
    where
      specialMap (ls , w, rs)
                 | w == v      = []
                 | w == l      = [(ls, w, r:(delete v rs))]
                 | w == r      = [(l:(delete v ls), w, rs)]
                 | otherwise   = [(ls, w, rs)]
joinOver _ _ = error "Can only join over a non-special vertex"



getLabels :: Graph -> [Int]
getLabels g = [ v | (_,v,_) <- g ]


relabelGraph :: [(Int, Int)] -> Graph -> Graph
relabelGraph p g = sortBy vertexOrd [ (sort $ map f l, f v, sort $ map f r) | (l,v,r) <- g ]
    where
      f n = fromJust $ lookup n p
                

graphIsomorphisms :: Graph -> Graph -> [[(Int, Int)]]
graphIsomorphisms g h
    | (length g) /= (length h) = []
    | otherwise                = filter iso
                                 $ map concat
                                 $ sequence
                                 $ zipWith getPermutations (sorted g) (sorted h)
    where
      sorted j = groupBy (\x y -> edgeOrd x y == EQ) $ sortBy edgeOrd j
      
      getPermutations l r = if (length l) /= (length r)
                            then []
                            else map (zip (getLabels l)) $ permutations (getLabels r)

      iso p = (simplifyLabels h) == (relabelGraph p g)


vertexOrd :: Vertex -> Vertex -> Ordering
(_,v,_) `vertexOrd` (_,w,_)   = compare v w

                                
edgeOrd :: Vertex -> Vertex -> Ordering
(l,_,r) `edgeOrd`   (l',_,r')
        | sl < sl' || (sl == sl' && sr < sr')  = LT
        | sl == sl' && sr == sr'               = EQ
        | otherwise                            = GT
    where 
      sl  = length l
      sl' = length l'
      sr  = length r
      sr' = length r'


rigid :: Graph -> Bool
rigid g = length (graphIsomorphisms g g) == 1


homeomorphic :: Graph -> Graph -> Bool
homeomorphic g h = not $ null $ graphIsomorphisms (graphReduction g) (graphReduction h)


homeomorphismClasses :: [Graph] -> [[Graph]]
homeomorphismClasses [] = []
homeomorphismClasses (g:gs) = let (l,r) = partition (homeomorphic g) (gs)
                              in  (g:l) : (homeomorphismClasses r)
