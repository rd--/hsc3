module Sound.SC3.UGen.Dot (draw, draw') where

import Sound.SC3.UGen.UGen (Name, UGen(..), isUGen, isProxy, isConstant, isControl)
import Sound.SC3.UGen.Rate (Rate(..))
import Sound.SC3.UGen.Operator (unaryName, binaryName)
import Sound.SC3.UGen.Graph (Graph(Graph), nindex, cindex, uindex, graph)

import Control.Exception (bracket)
import Data.List (intersperse)
import System.IO (openFile, hClose, hPutStr, IOMode(WriteMode))
import System.Cmd (system)
import System.Directory (getTemporaryDirectory)
import System.Environment (getEnv)

type UTerminal = (UGen, Int)
type UEdge     = (UTerminal, UTerminal)

uTerminal :: UGen -> UTerminal
uTerminal (Proxy u n)        = (u,n)
uTerminal u                  = (u,0)

uEdges :: UGen -> [UEdge]
uEdges u@(UGen _ _ i _ _ _) = map f i'
    where g (v,_)  = or [isUGen v, isProxy v, isControl v]
          n        = length i - 1
          i'       = filter g $ zip i [0..n]
          f (k, j) = (uTerminal k, (u, j))
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

edot :: (Show b) =>
        Graph -> ((UGen, b), (UGen, b)) -> [Char]
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
uname "UnaryOpUGen"   n = unaryName n
uname "BinaryOpUGen"  n = binaryName n
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

-- import Distribution.Compat.FilePath (joinPaths)
joinPaths :: FilePath -> FilePath -> FilePath
joinPaths a b = a ++ "/" ++ b

-- | Draw the UGen graph rooted at @u@ and display using the viewer @v@.
draw :: String {-^ graph root @u@ -} -> UGen {-^ viewer @v@ -} -> IO ()
draw v u = do d <- getTemporaryDirectory
              let f = joinPaths d "hsc.dot"
              bracket (openFile f WriteMode) hClose
                      (flip hPutStr (unlines (udot u)))
              system $ v ++ " " ++ f
              return ()

-- | Read the environment variable @DOTVIEWER@, the default value is @"dotty"@.
getDotViewer :: IO String
getDotViewer = catch (getEnv "DOTVIEWER") (\_ -> return "dotty")

-- | Draw the UGen graph rooted at @u@ using the viewer at 'getDotViewer'.
draw' :: UGen {-^ graph root @u@ -} -> IO ()
draw' u = getDotViewer >>= (flip draw u)
