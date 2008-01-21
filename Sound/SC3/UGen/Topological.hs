module Sound.SC3.UGen.Topological (tsort) where

import Data.List
import Sound.SC3.UGen.UGen (UGen)
import Sound.SC3.UGen.UGen.Predicate

-- Topological sort.

type D = (Int, UGen)

setDepth :: Int -> [UGen] -> [D] -> [D]
setDepth d u l = map f l
    where f (d', u') = if u' `elem` u then (max d d', u') else (d', u')

rsort :: Int -> [UGen] -> [D] -> [D]
rsort d r l = if null i then l else rsort (d + 1) i (setDepth d i l)
    where i = concatMap (filter isUGen . ugenInputs) r

tsort :: UGen -> [UGen] -> [UGen]
tsort r u = reverse (map snd (sortBy cmp (rsort 1 [r] (map ((,) 0) u))))
    where cmp (d1,_) (d2,_) = compare d1 d2
