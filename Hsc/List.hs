module Hsc.List where

import Data.Maybe (fromJust)
import Data.List  (nub, findIndex, elemIndex, transpose, find)

find' f      = fromJust . (find f)
findIndex' f = fromJust . (findIndex f)
elemIndex' e = fromJust . (elemIndex e)

nub' l = nub (reverse l)

iota n i s = take n (iterate (+ s) i)

interleave l = concat (transpose l)

f >.> g = g . f
compose = foldl (>.>) id
