{- | Plain notation for SuperCollider Ugen graphs.

> s = ugen "SinOsc" ar [440,0] 1
> m = binop CI "*" ar s 0.1
> o = ugen "Out" ar [0,m] 0
> map ugen_concise_pp [s, m, o]
-}
module Sound.Sc3.Ugen.Plain where

import Sound.Sc3.Common.Base
import Sound.Sc3.Common.Math.Operator
import Sound.Sc3.Common.Rate

import Sound.Sc3.Ugen.Types

-- | Variant of 'mkUgen'.
mk_plain :: Rate -> String -> [Ugen] -> Int -> Special -> UgenId -> Ugen
mk_plain rt nm inp = mkUgen Nothing all_rates (Left rt) nm inp Nothing

{- | Construct unary operator.

> uop CI "Neg" ar 1
-}
uop :: Case_Rule -> String -> Rate -> Ugen -> Ugen
uop cr nm r p =
    case unaryIndex cr nm of
      Just s -> mk_plain r "UnaryOpUGen" [p] 1 (Special s) NoId
      Nothing -> error "uop"

{- | Construct binary operator.

> binop CI "*" ar 1 2 == binop CI "Mul" ar 1 2
> binop CS "*" ar (ugen "SinOsc" ar [440,0] 1) 0.1 == sinOsc ar 440 0 * 0.1
-}
binop :: Case_Rule -> String -> Rate -> Ugen -> Ugen -> Ugen
binop cr nm r p q =
    case binaryIndex cr nm of
      Just s -> mk_plain r "BinaryOpUGen" [p,q] 1 (Special s) NoId
      Nothing -> error "binop"

{- | Construct deterministic Ugen.

> let o = ugen "SinOsc" ar [440,0] 1
> o == sinOsc ar 440 0
> ugen "Out" ar [0, o] 0 == out 0 (sinOsc ar 440 0)
-}
ugen :: String -> Rate -> [Ugen] -> Int -> Ugen
ugen nm r i nc = mk_plain r nm i nc (Special 0) NoId

{- | Construct non-deterministic Ugen.

> import Sound.Sc3.Common.Uid
> binop CI "*" ar (nondet "WhiteNoise" (Uid (fromEnum 'α')) ar [] 1) 0.05
-}
nondet :: String -> UgenId -> Rate -> [Ugen] -> Int -> Ugen
nondet nm z r i nc = mk_plain r nm i nc (Special 0) z
