module Hsc.Dot where

import Hsc.UGen
import Hsc.Graph
import List (intersperse)
import System.IO
import System.Cmd (system)

type UTerminal = (UGen, Int)
type UEdge     = (UTerminal, UTerminal)

uterminal :: UGen -> UTerminal
uterminal (Proxy u n)        = (u,n)
uterminal u                  = (u,0)

uedges :: UGen -> [UEdge]
uedges u@(UGen _ _ i _ _ _) = map f i'
    where g (u,i)  = or [isUGen u, isProxy u]
          n        = length i - 1
          i'       = filter g $ zip i [0..n]
          f (i, j) = (uterminal i, (u, j))
uedges _                    = []

edges :: Graph -> [UEdge]
edges (Graph n c u) = concat (map uedges u)

bdot :: String -> String -> String -> String -> String
bdot lbl shp clr slt = lbl
                       ++ " [shape=\"" ++ shp ++ "\", "
                       ++ "color=\"" ++ clr ++ "\", "
                       ++ "label=\"" ++ slt ++ "\"];"

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

edot g ((lu, ln), (ru, rn)) = nlabel g lu ++ ":O_" ++ show ln
                              ++ " -> "
                              ++ nlabel g ru ++ ":I_" ++ show rn ++ ";"

ratecolor :: Rate -> String
ratecolor AR = "black"
ratecolor KR = "blue"
ratecolor IR = "yellow"

idot :: UGen -> Int -> String
idot (Constant n) i = show n
idot _            i = "<I_" ++ show i ++ ">"

uname :: String -> Int -> String
uname "UnaryOpUGen"   0 = "-"
uname "UnaryOpUGen"  17 = "midicps"
uname "BinaryOpUGen"  0 = "+"
uname "BinaryOpUGen"  1 = "-"
uname "BinaryOpUGen"  2 = "*"
uname "BinaryOpUGen"  4 = "/"
uname n               _ = n

ndot :: Graph -> UGen -> String
ndot g u@(UGen r n i o s id) = rdot lbl clr [upr,lwr]
    where lbl = nlabel g u
          clr = ratecolor r
          i'  = length i - 1
          upr = uname n s : zipWith (\j k -> idot j k) i [0..i']
          o'  = length o - 1
          lwr = map (\j -> "<O_" ++ show j ++ ">") [0..o']

ndot g u@(Control r n d) = nlabel g u
                           ++ "[shape=\"trapezium\", color=\""
                           ++ ratecolor r
                           ++ "\",label=\""
                           ++ n ++ ":" ++ show (cindex g u)
                           ++ "];"

gdot :: Graph -> [String]
gdot g@(Graph n c u) = ["digraph Anonymous {"]
                       ++ map (ndot g) c
                       ++ map (ndot g) u
                       ++ map (edot g) (edges g)
                       ++ ["}"]

draw u = do h <- openFile f WriteMode
            mapM (hPutStrLn h) (gdot g)
            hClose h
            system ("open " ++ f)
    where g = graph u
          f = "/tmp/hsc.dot"
