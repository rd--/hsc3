-- | Plain notation for SuperCollider UGen graphs.
--
-- > s = ugen "SinOsc" AR [440,0] 1
-- > m = binop CI "*" AR s 0.1
-- > o = ugen "Out" AR [0,m] 0
module Sound.SC3.UGen.Plain where

import Sound.SC3.Common.Base
import Sound.SC3.Common.Math.Operator
import Sound.SC3.Common.Rate
import Sound.SC3.UGen.Type

-- | Variant of 'mkUGen'.
mk_plain :: Rate -> String -> [UGen] -> Int -> Special -> UGenId -> UGen
mk_plain r nm inp = mkUGen Nothing all_rates (Left r) nm inp Nothing

-- | Construct unary operator, the name can textual or symbolic.
--
-- > uop CI "NEG" AR 1
uop :: Case_Rule -> String -> Rate -> UGen -> UGen
uop cr nm r p =
    case unaryIndex cr nm of
      Just s -> mk_plain r "UnaryOpUGen" [p] 1 (Special s) NoId
      Nothing -> error "uop"

-- | Construct binary operator, the name can textual or symbolic.
--
-- > binop CI "*" AR 1 2 == binop CI "MUL" AR 1 2
-- > binop CS "*" AR (ugen "SinOsc" AR [440,0] 1) 0.1 == sinOsc AR 440 0 * 0.1
binop :: Case_Rule -> String -> Rate -> UGen -> UGen -> UGen
binop cr nm r p q =
    case binaryIndex cr nm of
      Just s -> mk_plain r "BinaryOpUGen" [p,q] 1 (Special s) NoId
      Nothing -> error "binop"

-- | Construct deterministic UGen.
--
-- > let o = ugen "SinOsc" AR [440,0] 1
-- > o == sinOsc AR 440 0
-- > ugen "Out" AR [0,o] 0 == out 0 (sinOsc AR 440 0)
ugen :: String -> Rate -> [UGen] -> Int -> UGen
ugen nm r i nc = mk_plain r nm i nc (Special 0) NoId

-- | Construct non-deterministic UGen.
--
-- > import Sound.SC3.Common.UId
-- > binop CI "*" AR (nondet "WhiteNoise" (UId (fromEnum 'a')) AR [] 1) 0.05
nondet :: String -> UGenId -> Rate -> [UGen] -> Int -> UGen
nondet nm z r i nc = mk_plain r nm i nc (Special 0) z
