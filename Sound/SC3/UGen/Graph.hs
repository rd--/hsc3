module Sound.SC3.UGen.Graph (Graph(..), Input(..),
                             graph,
                             controlIndex, constantIndex, ugenIndex,
                             makeInput) where

import Sound.SC3.UGen.UGen (UGen(..), UId(UId), nodes, isConstant, isControl, isUGen)
import Sound.SC3.UGen.Rate (Rate(KR))

import Data.Maybe (fromMaybe)
import Data.List (nub, elemIndex)

data Graph = Graph [UGen] [UGen] [UGen]
             deriving (Eq, Show)

data Input = Input Int Int
             deriving (Eq, Show)

-- | Construct implicit control UGen (k-rate only).
implicit :: Int -> UGen
implicit n = UGen KR "Control" [] (replicate n KR) 0 (UId 0)

-- | Construct a UGen graph.
graph :: UGen -> Graph
graph root = Graph n c u'
  where e  = (nub . reverse) (nodes root)
        n  = filter isConstant e
        c  = filter isControl e
        u  = filter isUGen e
        u' = if null c then u else implicit (length c) : u

-- | Determine index of a node in the Graph.
nodeIndex :: (Eq a, Show a) => a -> [a] -> Int
nodeIndex e l = fromMaybe (error ("node not in graph?" ++ show (e,l))) 
                          (elemIndex e l)

-- | Determine index of UGen in Graph.
ugenIndex :: Graph -> UGen -> Int
ugenIndex (Graph _ _ u) x = nodeIndex x u

-- | Determine index of Constant in Graph.
constantIndex :: Graph -> UGen -> Int
constantIndex (Graph n _ _) x = nodeIndex x n

-- | Determine index of Control in Graph.
controlIndex :: Graph -> UGen -> Int
controlIndex (Graph _ c _) x = nodeIndex x c

-- | Construct Input value for UGen in Graph.
makeInput :: Graph -> UGen -> Input
makeInput g u@(UGen _ _ _ _ _ _) = Input (ugenIndex g u) 0
makeInput g u@(Constant _)       = Input (-1) (constantIndex g u)
makeInput g u@(Control _ _ _)    = Input 0 (controlIndex g u)
makeInput g (Proxy u n)          = Input (ugenIndex g u) n
makeInput g u                    = error ("makeInput: illegal input: " ++ show (g,u))
