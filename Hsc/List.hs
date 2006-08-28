module Hsc.List where

import Data.Maybe (fromJust)
import Data.List  (nub, findIndex, elemIndex, transpose, find)

find' :: (a -> Bool) -> [a] -> a
find' f      = fromJust . find f

findIndex' :: (a -> Bool) -> [a] -> Int
findIndex' f = fromJust . findIndex f

elemIndex' :: (Eq a) => a -> [a] -> Int
elemIndex' e = fromJust . elemIndex e

nub' :: Eq a => [a] -> [a]
nub' l = nub (reverse l)

iota :: (Num a) => Int -> a -> a -> [a]
iota n i s = take n (iterate (+ s) i)

interleave :: [[a]] -> [a]
interleave l = concat (transpose l)

compose :: [t -> t] -> t -> t
compose = foldl (>.>) id

(>.>) :: (a -> b) -> (b -> c) -> (a -> c)
(>.>) = flip (.)
