module Sound.SC3.Server.Graphdef (graphdef) where

import Sound.SC3.UGen.UGen (UGen(..))
import Sound.SC3.UGen.Rate (rateId)
import Sound.SC3.UGen.Graph
import Sound.SC3.Server.U8v

input_u8v :: Input -> [U8]
input_u8v (Input u p) = i16_u8v u ++ i16_u8v p

ugen_u8v :: Graph -> UGen -> [U8]
ugen_u8v g c@(Control _ n _)    = pstr_u8v n ++ i16_u8v (cindex g c)
ugen_u8v g (UGen r n i o s _)   = pstr_u8v n ++
                                  i8_u8v (rateId r) ++
                                  i16_u8v (length i) ++
                                  i16_u8v (length o) ++
                                  i16_u8v s ++
                                  concatMap (input_u8v . mkInput g) i ++
                                  concatMap (i8_u8v . rateId) o
ugen_u8v _ _                    = error "illegal input"

-- | Construct instrument definition bytecode.
graphdef :: String -> Graph -> [U8]
graphdef s g@(Graph n c u) = str_u8v "SCgf" ++
                             i32_u8v 0 ++
                             i16_u8v 1 ++
                             pstr_u8v s ++
                             i16_u8v (length n) ++
                             concatMap (f32_u8v . f64_f32 . nvalue) n ++
                             i16_u8v (length c) ++
                             concatMap (f32_u8v . f64_f32 . cdefault) c ++
                             i16_u8v (length c) ++
                             concatMap (ugen_u8v g) c ++
                             i16_u8v (length u) ++
                             concatMap (ugen_u8v g) u
