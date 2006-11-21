module Sound.SC3.UGen.Graph where

import Sound.SC3.UGen.UGen (UGen(..), UId(UId), nodes, isConstant, isControl, isUGen)
import Sound.SC3.UGen.Rate (Rate(KR))

import Data.Maybe (fromMaybe)
import Data.List (nub, elemIndex)

data Graph = Graph [UGen] [UGen] [UGen]
             deriving (Eq, Show)

data Input = Input Int Int
             deriving (Eq, Show)

implicit :: Int -> UGen
implicit n = UGen KR "Control" [] (replicate n KR) 0 (UId 0)

graph :: UGen -> Graph
graph root = Graph n c u'
  where e  = (nub . reverse) (nodes root)
        n  = filter isConstant e
        c  = filter isControl e
        u  = filter isUGen e
        u' = if null c then u else implicit (length c) : u

elemIndex' :: (Eq a, Show a) => a -> [a] -> Int
elemIndex' e l =
   fromMaybe (error ("index search failed" ++ show (e,l)))
             (elemIndex e l)

uindex :: Graph -> UGen -> Int
uindex (Graph _ _ u) x = elemIndex' x u

cindex :: Graph -> UGen -> Int
cindex (Graph _ c _) x = elemIndex' x c

nindex :: Graph -> UGen -> Int
nindex (Graph n _ _) x = elemIndex' x n

mkInput :: Graph -> UGen -> Input
mkInput g u@(UGen _ _ _ _ _ _) = Input (uindex g u) 0
mkInput g u@(Constant _)       = Input (-1) (nindex g u)
mkInput g u@(Control _ _ _)    = Input 0 (cindex g u)
mkInput g (Proxy u n)          = Input (uindex g u) n
mkInput g u                    = error ("mkInput: illegal input: " ++ show (g,u))

nvalue :: UGen -> Double
nvalue   (Constant n)    = n
nvalue   _               = error "nvalue: non constant input"

cdefault :: UGen -> Double
cdefault (Control _ _ n) = n
cdefault  _              = error "cdefault: non control input"
