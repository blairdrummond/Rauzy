import Rauzy

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.CmdLine 
 
getOptions :: String -> Int -> Diagram B
getOptions "thueMorse"       = rauzyGraph' (subToRauzy thueMorseSystem)
getOptions "fibonacci"       = rauzyGraph' (subToRauzy fibonacciSystem)
getOptions "npChacon"        = rauzyGraph' (subToRauzy npChaconSystem)
getOptions "tribonacci"      = rauzyGraph' (subToRauzy tribonacciSystem)

getOptions "fullThueMorse"   = rauzyGraph  (subToRauzy thueMorseSystem)
getOptions "fullFibonacci"   = rauzyGraph  (subToRauzy fibonacciSystem)
getOptions "fullNpChacon"    = rauzyGraph  (subToRauzy npChaconSystem)
getOptions "fullTribonacci"  = rauzyGraph  (subToRauzy tribonacciSystem)

getOptions    _              = error "Choose either fibonacci, thueMorse, or npChacon"

main = mainWith getOptions
