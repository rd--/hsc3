module Hsc.Dot (draw, draw') where

import Hsc.UGen
import Hsc.Rate
import Hsc.Graph
import Hsc.Operator
import Data.List (intersperse)
import System.IO
import System.Cmd (system)
import System.Directory (getTemporaryDirectory)
import System.Environment (getEnv)
import Control.Exception (bracket)

type UTerminal = (UGen, Int)
type UEdge     = (UTerminal, UTerminal)

uTerminal :: UGen -> UTerminal
uTerminal (Proxy u n)        = (u,n)
uTerminal u                  = (u,0)

uEdges :: UGen -> [UEdge]
uEdges u@(UGen _ _ i _ _ _) = map f i'
    where g (u,_)  = or [isUGen u, isProxy u, isControl u]
          n        = length i - 1
          i'       = filter g $ zip i [0..n]
          f (i, j) = (uTerminal i, (u, j))
uEdges _                    = []

edges :: Graph -> [UEdge]
edges (Graph _ _ u) = concatMap uEdges u

rdot :: String -> String -> [[String]] -> String
rdot lbl clr slt = lbl
                   ++ " [shape=\"record\", "
                   ++ "color=\"" ++ clr ++ "\", "
                   ++ "label=\"{" ++ concat (g (map f slt)) ++ "}\"];"
    where f l = "{" ++ concat (g l) ++ "}"
          g l = (intersperse "|" l)

nlabel :: Graph -> UGen -> String
nlabel g u
    | isConstant u = "N_" ++ show (nindex g u)
    | isControl  u = "C_" ++ show (cindex g u)
    | isUGen     u = "U_" ++ show (uindex g u)
    | otherwise    = error "nlabel: illegal input"

-- This is broken for Control edges...

edot g ((lu, ln), (ru, rn)) = nlabel g lu ++ ":O_" ++ show ln
                              ++ " -> "
                              ++ nlabel g ru ++ ":I_" ++ show rn ++ ";"

rateColor :: Rate -> String
rateColor AR = "black"
rateColor KR = "blue"
rateColor IR = "yellow"
rateColor DR = "red"

idot :: UGen -> Int -> String
idot (Constant n) _ = show n
idot _            i = "<I_" ++ show i ++ ">"


uname :: Name -> Int -> String
uname "UnaryOpUGen"   n = uOpName n
uname "BinaryOpUGen"  n = bOpName n
uname n               _ = n

ndot :: Graph -> UGen -> String
ndot g u@(UGen r n i o s _) = rdot lbl clr [upr,lwr]
    where lbl = nlabel g u
          clr = rateColor r
          i'  = length i - 1
          upr = uname n s : zipWith idot i [0..i']
          o'  = length o - 1
          lwr = map (\j -> "<O_" ++ show j ++ ">") [0..o']
ndot g u@(Control r n v)    = nlabel g u
                              ++ "[shape=\"trapezium\", color=\""
                              ++ rateColor r
                              ++ "\",label=\""
                              ++ n ++ ":" ++ show v
                              ++ "\"];"
ndot _ _                    = error "ndot: illegal input"

gdot :: Graph -> [String]
gdot g@(Graph _ c u) = ["digraph Anonymous {"]
                       ++ map (ndot g) c
                       ++ map (ndot g) u
                       ++ map (edot g) (edges g)
                       ++ ["}"]

udot :: UGen -> [String]
udot u = gdot (graph u)

--import Distribution.Compat.FilePath (joinPaths)
joinPaths a b = a ++ "/" ++ b

draw :: String -> UGen -> IO ()
draw v u = do d <- getTemporaryDirectory
              let f = joinPaths d "hsc.dot"
              bracket (openFile f WriteMode) hClose
                      (flip hPutStr (unlines (udot u)))
              system $ v ++ " " ++ f
              return ()


getDotViewer = catch (getEnv "DOTVIEWER") (\_ -> return "dotty")

draw' :: UGen -> IO ()
draw' u = do v <- getDotViewer
             draw v u
