{-# LANGUAGE TypeFamilies        #-}

module Rauzy.Diagrams
    (
     RauzyWord
     , subToRauzy
     , rauzyGraph
     , rauzyGraph'
    ) where

import Data.List
import Data.Maybe
import Prelude hiding (Word)
import Options.Applicative

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.CmdLine 

import Rauzy.Alphabet
import Rauzy.Substitution





    
type RauzyWord = (String, [String], Int -> [String], Int -> [(String, String, String)])

subToRauzy :: Alphabet a => Substitution a -> RauzyWord
subToRauzy w@(s,i) = (limitWord, alph, vertices, edges)
  where
    limitWord = toStr $ limit w
    alph = map show $ alphabet i
    size = length alph
    vertices n = [ toStr $ toEnum x `asTypeOf` [i] | x <- [size^n - 1 .. size^(n+1) - 2]]
    edges n = [ (toStr a, toStr b, show c) | (a,b,c) <- (subToEdges w !! (n - 1)) ]



rauzyGraph' :: RauzyWord -> Int -> Diagram B
rauzyGraph' (l,alph, _ ,edgeFun) = rauzyGraph (l,alph, appearing, edgeFun)
    where
      appearing n = let (tos,froms,_) = unzip3 $ edgeFun n
                    in sort $ nub $ tos ++ froms
              
rauzyGraph :: RauzyWord -> Int -> Diagram B
rauzyGraph w@(_,alph,vertexFun,edgeFun) n = (atPoints (trailVertices $ regPoly (length vertexList) 1) (map node vertexList)
  # applyAll [ connectOutside' ((arrowOpts !! alphaIndex z) x y) (index x) (index y)                        | (x,y,z) <- edgeList, x /= y ]
  # applyAll [ connectOutside'  ((headOpts !! alphaIndex z) x y) (index x) (index y)                        | (x,y,z) <- edgeList, x /= y ]
  # applyAll [ connectPerim'     (loopOpts !! alphaIndex z)      (index x) (index y) (loopIn x) (loopOut x) | (x,y,z) <- edgeList, x == y ]) `atop` square ((fromIntegral $ length vertexList) / pi + (fromIntegral n) / 3 + 0.5) # lw 0
  where
    vertexList      = vertexFun n 
    edgeList        = edgeFun n
    index s         = fromJust $ elemIndex s vertexList
    alphaIndex z    = fromJust $ elemIndex z (alph)
    (tos, froms, _) = unzip3 edgeList
    lonely = vertexList \\ (tos ++ froms)

    node s = text s # fontSizeL (0.2 / (sqrt (fromIntegral n))) # italic # fc black # heavy # font "freeserif" # lw none
             <> circle 0.2 # (if s `elem` lonely then fc coral # lc crimson else fc silver) # named (index s) # lw (local 0.01)

    loopIn  x = ((fromIntegral . index) x) / (fromIntegral $ length vertexList) - 1/12 @@ turn
    loopOut x = ((fromIntegral . index) x) / (fromIntegral $ length vertexList) + 1/12 @@ turn
                
    nonZero x
        | -0.1 <  x && x <= 0  = -0.05
        |   0  <= x && x < 0.1 =  0.05
        | otherwise            =  x
                
    orientation a1 a2 = if n == 1
                        then (1/6 @@ turn)
                        else 
                            if a1 <= a2
                            then - nonZero (cos ((a2 - a1) / (fromIntegral $ length vertexList) * pi ) / 3) @@ turn
                            else   nonZero (cos ((a2 - a1) / (fromIntegral $ length vertexList) * pi ) / 3) @@ turn
    
    arrowOpts = [ (\x y -> with & arrowHead .~ dart
                                & headStyle %~ fc color . opacity 0
                                & shaftStyle %~ lc color . opacity 0.5
                                & arrowShaft .~ arc xDir (orientation (fromIntegral $ index x) (fromIntegral $ index y))
                                & headLength .~ normal) | color <- colours ]

    headOpts = [ (\x y -> with & arrowHead .~ dart
                               & headStyle %~ fc color . opacity 1
                               & shaftStyle %~ lc color . opacity 0
                               & arrowShaft .~ arc xDir (orientation (fromIntegral $ index x) (fromIntegral $ index y))
                               & headLength .~ normal) | color <- colours ]

    loopOpts = [ with & arrowHead  .~ dart
                      & headStyle %~ fc color
                      & shaftStyle %~ lc color
                      & arrowShaft .~ arc xDir (2/3 @@ turn)
                      & lengths .~ normal | color <- colours ]

 

colours = cycle [
    teal
-- , silver
  , crimson
  , orange
  , green
  , orchid
  , royalblue
  , deepskyblue
  , springgreen
  , linen
  , tomato
  , rosybrown
  , cyan
  , yellow
  , forestgreen
  , maroon
  , red
  , thistle
  , blueviolet
  , deeppink
  , indianred
  , brown
  , lavenderblush
 -- , coral
  , indigo
  , powderblue
  , seashell
  , burlywood
  , purple
  , hotpink
  , magenta
  , saddlebrown
  , salmon
  , skyblue
  , lime
  , lemonchiffon
  , turquoise
  , fuchsia
  , gold
  , peru
  , lawngreen
  , cornflowerblue
  , violet
  , chartreuse
  , ivory
  , cadetblue
  , honeydew
  , firebrick
  , sienna
  , yellowgreen
  ]
