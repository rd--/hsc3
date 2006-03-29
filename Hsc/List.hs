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

findIndex' :: (a -> Bool) -> [a] -> Int
findIndex' f l = fromJust $ findIndex f l

elemIndex' :: (Eq a) => a -> [a] -> Int
elemIndex' e l = fromJust $ elemIndex e l
