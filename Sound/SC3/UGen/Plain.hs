-- | Plain UGen constructor functions.
--
-- > let {s = ugen "SinOsc" AR [440,0] 1
-- >     ;m = binop "*" AR s 0.1
-- >     ;o = ugen "Out" AR [0,m] 0}
-- > in Sound.SC3.Server.Play.audition o >> Sound.SC3.UGen.Dot.draw o
--
-- > audition (out 0 (sinOsc AR 440 0 * 0.1))
module Sound.SC3.UGen.Plain where

import Sound.SC3.UGen.Operator
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.UGen

-- | Variant of 'mkUGen'.
mk_plain :: Rate -> String -> [UGen] -> Int -> Special -> UGenId -> UGen
mk_plain r = mkUGen Nothing all_rates (Just r)

-- | Construct unary operator, the name can textual or symbolic.
--
-- > uop "-" AR 1 == uop "Neg" AR 1
uop :: String -> Rate -> UGen -> UGen
uop nm r p =
    let s = unaryIndex nm
    in mk_plain r "UnaryOpUGen" [p] 1 (Special s) NoId

-- | Construct binary operator, the name can textual or symbolic.
--
-- > binop "*" AR 1 2 == binop "Mul" AR 1 2
-- > binop "*" AR (ugen "SinOsc" AR [440,0] 1) 0.1 == sinOsc AR 440 0 * 0.1
-- > ugenName (binop "*" AR 1 2) == "BinaryOpUGen"
binop :: String -> Rate -> UGen -> UGen -> UGen
binop nm r p q =
    let s = binaryIndex nm
    in mk_plain r "BinaryOpUGen" [p,q] 1 (Special s) NoId

-- | Construct deterministic UGen.
--
-- > let o = ugen "SinOsc" AR [440,0] 1
-- > o == sinOsc AR 440 0
-- > ugen "Out" AR [0,o] 0 == out 0 (sinOsc AR 440 0)
ugen :: String -> Rate -> [UGen] -> Int -> UGen
ugen nm r i nc = mk_plain r nm i nc (Special 0) NoId

-- | Construct non-deterministic UGen.
--
-- > import Sound.SC3.ID
-- > nondet "WhiteNoise" (UId (fromEnum 'a')) AR [] 1 == whiteNoise 'a' AR
nondet :: String -> UGenId -> Rate -> [UGen] -> Int -> UGen
nondet nm z r i nc = mk_plain r nm i nc (Special 0) z
