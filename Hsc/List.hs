module Hsc.List where

import Data.Maybe (fromJust)
import Data.List

instance Num a => Num [a] where
    negate         = map negate
    (+)            = zipWith (+)
    (-)            = zipWith (-)
    (*)            = zipWith (*)
    abs            = map abs
    signum         = map signum
    fromInteger _  = []

uniq :: (Eq a) => [a] -> [a]
uniq []     = []
uniq (x:xs) = if elem x xs then uniq xs else x : uniq xs

findIndex' :: (a -> Bool) -> [a] -> Int
findIndex' f l = fromJust $ findIndex f l

elemIndex' :: (Eq a) => a -> [a] -> Int
elemIndex' e l = fromJust $ elemIndex e l

-- Unsafe...

invert :: [[a]] -> [[a]]
invert l = map (\n -> (map (!! n) l)) [0..n]
    where n = (length (head l)) - 1
