module Hsc.Dot (draw, draw') where

import Hsc.UGen
import Hsc.Rate
import Hsc.Graph
import Data.List (intersperse)
import System.IO
import System.Cmd (system)
import System.Directory (getTemporaryDirectory)
import System.Posix.Env (getEnvDefault)

type UTerminal = (UGen, Int)
type UEdge     = (UTerminal, UTerminal)

uterminal :: UGen -> UTerminal
uterminal (Proxy u n)        = (u,n)
uterminal u                  = (u,0)

uedges :: UGen -> [UEdge]
uedges u@(UGen _ _ i _ _ _) = map f i'
    where g (u,_)  = or [isUGen u, isProxy u]
          n        = length i - 1
          i'       = filter g $ zip i [0..n]
          f (i, j) = (uterminal i, (u, j))
uedges _                    = []

edges :: Graph -> [UEdge]
edges (Graph _ _ u) = concatMap uedges u

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
ratecolor DR = "red"

idot :: UGen -> Int -> String
idot (Constant n) _ = show n
idot _            i = "<I_" ++ show i ++ ">"

uname :: Name -> Int -> String
uname "UnaryOpUGen"   0 = "-"
uname "UnaryOpUGen"   1 = "not"
uname "UnaryOpUGen"   2 = "isnil"
uname "UnaryOpUGen"   3 = "notnil"
uname "UnaryOpUGen"   4 = "bitnot"
uname "UnaryOpUGen"   5 = "abs"
uname "UnaryOpUGen"   6 = "asfloat"
uname "UnaryOpUGen"   7 = "asint"
uname "UnaryOpUGen"   8 = "ceil"
uname "UnaryOpUGen"   9 = "floor"
uname "UnaryOpUGen"  10 = "frac"
uname "UnaryOpUGen"  11 = "signum"
uname "UnaryOpUGen"  12 = "squared"
uname "UnaryOpUGen"  13 = "cubed"
uname "UnaryOpUGen"  14 = "sqrt"
uname "UnaryOpUGen"  15 = "exp"
uname "UnaryOpUGen"  16 = "recip"
uname "UnaryOpUGen"  17 = "midicps"
uname "UnaryOpUGen"  18 = "cpsmidi"
uname "UnaryOpUGen"  19 = "midiratio"
uname "UnaryOpUGen"  20 = "ratiomidi"
uname "UnaryOpUGen"  21 = "dbamp"
uname "UnaryOpUGen"  22 = "ampdb"
uname "UnaryOpUGen"  23 = "octcps"
uname "UnaryOpUGen"  24 = "cpsoct"
uname "UnaryOpUGen"  25 = "log"
uname "UnaryOpUGen"  26 = "log2"
uname "UnaryOpUGen"  27 = "log10"
uname "BinaryOpUGen"  0 = "+"
uname "BinaryOpUGen"  1 = "-"
uname "BinaryOpUGen"  2 = "*"
uname "BinaryOpUGen"  4 = "/"
uname "BinaryOpUGen"  5 = "/"
uname "BinaryOpUGen"  6 = "/"
uname "BinaryOpUGen"  7 = "/"
uname "BinaryOpUGen"  8 = "/"
uname "BinaryOpUGen"  9 = "/"
uname "BinaryOpUGen" 10 = "/"
uname "BinaryOpUGen" 25 = "**"
uname n               _ = n

ndot :: Graph -> UGen -> String
ndot g u@(UGen r n i o s _) = rdot lbl clr [upr,lwr]
    where lbl = nlabel g u
          clr = ratecolor r
          i'  = length i - 1
          upr = uname n s : zipWith (\j k -> idot j k) i [0..i']
          o'  = length o - 1
          lwr = map (\j -> "<O_" ++ show j ++ ">") [0..o']

ndot g u@(Control r n _) = nlabel g u
                           ++ "[shape=\"trapezium\", color=\""
                           ++ ratecolor r
                           ++ "\",label=\""
                           ++ n ++ ":" ++ show (cindex g u)
                           ++ "];"

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

draw' :: UGen -> IO ()
draw' u = do v <- getEnvDefault "DOTVIEWER" "dotty"
             draw v u
