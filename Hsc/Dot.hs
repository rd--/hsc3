module Hsc.Dot (draw, draw') where

import Hsc.UGen
import Hsc.Rate
import Hsc.Graph
import Data.List (intersperse)
import System.IO
import System.Cmd (system)
import System.Directory (getTemporaryDirectory)
import System.Environment (getEnv)

type UTerminal = (UGen, Int)
type UEdge     = (UTerminal, UTerminal)

uterminal :: UGen -> UTerminal
uterminal (Proxy u n)        = (u,n)
uterminal u                  = (u,0)

uedges :: UGen -> [UEdge]
uedges u@(UGen _ _ i _ _ _) = map f i'
    where g (u,_)  = or [isUGen u, isProxy u, isControl u]
          n        = length i - 1
          i'       = filter g $ zip i [0..n]
          f (i, j) = (uterminal i, (u, j))
uedges _                    = []

edges :: Graph -> [UEdge]
edges (Graph _ _ u) = concatMap uedges u

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

ratecolor :: Rate -> String
ratecolor AR = "black"
ratecolor KR = "blue"
ratecolor IR = "yellow"
ratecolor DR = "red"

idot :: UGen -> Int -> String
idot (Constant n) _ = show n
idot _            i = "<I_" ++ show i ++ ">"

uopname  0 = "-"
uopname  1 = "not"
uopname  2 = "isnil"
uopname  3 = "notnil"
uopname  4 = "bitnot"
uopname  5 = "abs"
uopname  6 = "asfloat"
uopname  7 = "asint"
uopname  8 = "ceil"
uopname  9 = "floor"
uopname 10 = "frac"
uopname 11 = "signum"
uopname 12 = "squared"
uopname 13 = "cubed"
uopname 14 = "sqrt"
uopname 15 = "exp"
uopname 16 = "recip"
uopname 17 = "midicps"
uopname 18 = "cpsmidi"
uopname 19 = "midiratio"
uopname 20 = "ratiomidi"
uopname 21 = "dbamp"
uopname 22 = "ampdb"
uopname 23 = "octcps"
uopname 24 = "cpsoct"
uopname 25 = "log"
uopname 26 = "log2"
uopname 27 = "log10"
uopname _  = "Unknown Unary Op"

binopname  0 = "+"
binopname  1 = "-"
binopname  2 = "*"
binopname  3 = "div"
binopname  4 = "/"
binopname  5 = "%"
binopname  6 = "=="
binopname  7 = "/="
binopname  8 = "<"
binopname  9 = ">"
binopname 10 = "<="
binopname 11 = ">="
binopname 12 = "min"
binopname 13 = "max"
binopname 14 = "bitand"
binopname 15 = "bitor"
binopname 16 = "bitxor"
binopname 17 = "lcm"
binopname 18 = "gcd"
binopname 19 = "round"
binopname 20 = "roundup"
binopname 21 = "trunc"
binopname 22 = "atan2"
binopname 23 = "hypot"
binopname 24 = "hypotx"
binopname 25 = "**"
binopname 26 = "shiftleft"
binopname 27 = "shiftright"
binopname 28 = "unsignedshift"
binopname _  = "Unknown Binary Op"

uname :: Name -> Int -> String
uname "UnaryOpUGen"   n = uopname   n
uname "BinaryOpUGen"  n = binopname n
uname n               _ = n

ndot :: Graph -> UGen -> String
ndot g u@(UGen r n i o s _) = rdot lbl clr [upr,lwr]
    where lbl = nlabel g u
          clr = ratecolor r
          i'  = length i - 1
          upr = uname n s : zipWith (\j k -> idot j k) i [0..i']
          o'  = length o - 1
          lwr = map (\j -> "<O_" ++ show j ++ ">") [0..o']
ndot g u@(Control r n v)    = nlabel g u
                              ++ "[shape=\"trapezium\", color=\""
                              ++ ratecolor r
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

draw :: String -> UGen -> IO ()
draw v u = do d <- getTemporaryDirectory
              f <- return (d ++ "/hsc.dot")
              h <- openFile f WriteMode
              mapM (hPutStrLn h) (udot u)
              hClose h
              system $ v ++ " " ++ f
              return ()


getDotViewer = catch (getEnv "DOTVIEWER") (\_ -> return "dotty")

draw' :: UGen -> IO ()
draw' u = do v <- getDotViewer
             draw v u
