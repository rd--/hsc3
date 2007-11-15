module Sound.SC3.UGen.Graph ( Graph(..), Input(..), Terminal(..), Edge(..)
                            , graph
                            , nodeIndex
                            , makeInput ) where

import Sound.SC3.UGen.Rate (Rate(KR))
import Sound.SC3.UGen.UGen (UGen(..), Special(..), UGenId(..))
import Sound.SC3.UGen.UGen.Predicate

import Data.Maybe (fromMaybe)
import Data.List (nub, elemIndex)

data Terminal = Terminal UGen Int deriving (Eq, Show)
data Edge = Edge Terminal Terminal deriving (Eq, Show)
data Graph = Graph [UGen] [UGen] [UGen] [Edge] deriving (Eq, Show)
data Input = Input Int Int deriving (Eq, Show)

-- | The list of all UGens referenced in a UGen graph.
nodes :: UGen -> [UGen]
nodes u@(UGen _ _ i _ _ _) = u : concatMap nodes i
nodes (Proxy u _)          = u : nodes u
nodes (MCE u)              = concatMap nodes u
nodes (MRG u)              = concatMap nodes u
nodes u                    = [u]

-- | Construct implicit control UGen (k-rate only).
implicit :: Int -> UGen
implicit n = UGen KR "Control" [] (replicate n KR) (Special 0) (UGenId 0)

-- | Generate the set of edges given the complete set of UGens.
edges :: [UGen] -> [Edge]
edges us = concatMap ugenEdges us
    where ugenEdges u@(UGen _ _ i _ _ _) = map f i'
               where g (v,_) = or [isUGen v, isProxy v, isControl v, isMRG v]
                     n = length i - 1
                     i' = filter g $ zip i [0..n]
                     f (k, j) = Edge (terminal k) (Terminal u j)
          ugenEdges _ = []

-- | Construct a UGen graph.
graph :: UGen -> Graph
graph root = Graph n c u' (edges u')
  where e  = (nub . reverse) (nodes root)
        n  = filter isConstant e
        c  = filter isControl e
        u  = filter isUGen e
        u' = if null c then u else implicit (length c) : u

-- | Determine index of a node in the Graph.
elemIndex' :: (Eq a, Show a) => a -> [a] -> Int
elemIndex' e l = fromMaybe (error ("node not in graph?" ++ show (e,l))) 
                 (elemIndex e l)
                 
-- | Determine index of UGen in Graph.
ugenIndex :: Graph -> UGen -> Int
ugenIndex (Graph _ _ u _) x = elemIndex' x u

-- | Determine index of Constant in Graph.
constantIndex :: Graph -> UGen -> Int
constantIndex (Graph n _ _ _) x = elemIndex' x n

-- | Determine index of Control in Graph.
controlIndex :: Graph -> UGen -> Int
controlIndex (Graph _ c _ _) x = elemIndex' x c

-- | Determine index of any node in Graph.
nodeIndex :: Graph -> UGen -> Int
nodeIndex g u@(Constant _) = constantIndex g u
nodeIndex g u@(Control _ _ _) = controlIndex g u
nodeIndex g u@(UGen _ _ _ _ _ _) = ugenIndex g u
nodeIndex g (MRG (u:_)) = ugenIndex g u
nodeIndex _ _ = error "nodeIndex: illegal input"

-- | Construct Input value for UGen in Graph.
makeInput :: Graph -> UGen -> Input
makeInput g u@(UGen _ _ _ _ _ _) = Input (ugenIndex g u) 0
makeInput g u@(Constant _)       = Input (-1) (constantIndex g u)
makeInput g u@(Control _ _ _)    = Input 0 (controlIndex g u)
makeInput g (Proxy u n)          = Input (ugenIndex g u) n
makeInput g (MRG (u:_))          = makeInput g u
makeInput g u                    = error ("makeInput: illegal input: " ++ show (g,u))

-- | Construct a terminal value, the port index is set for proxied
-- | UGens.
terminal :: UGen -> Terminal
terminal (Proxy u n) = Terminal u n
terminal u = Terminal u 0
