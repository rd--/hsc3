-- | Plain UGen constructor functions.
module Sound.SC3.UGen.Plain where

import Sound.SC3.UGen.Operator
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.UGen

mk_plain :: Rate -> String -> [UGen] -> Int -> Special -> UGenId -> UGen
mk_plain r = mkUGen Nothing all_rates (Just r)

uop :: String -> Rate -> UGen -> UGen
uop nm r p =
    let s = unaryIndex nm
    in mk_plain r nm [p] 1 (Special s) NoId

binop :: String -> Rate -> UGen -> UGen -> UGen
binop nm r p q =
    let s = binaryIndex nm
    in mk_plain r nm [p,q] 1 (Special s) NoId

ugen :: String -> Rate -> [UGen] -> Int -> UGen
ugen nm r i nc = mk_plain r nm i nc (Special 0) NoId

nondet :: String -> UGenId -> Rate -> [UGen] -> Int -> UGen
nondet nm z r i nc = mk_plain r nm i nc (Special 0) z
