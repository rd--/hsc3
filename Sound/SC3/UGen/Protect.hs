-- | Functions to re-write assigned node identifiers at UGen graphs.
-- Used carefully it allows for composition of sub-graphs with
-- psuedo-random nodes.
module Sound.SC3.UGen.Protect where

import Sound.SC3.UGen.Identifier
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UGen

-- | Collect Ids at UGen graph
ugenIds :: UGen -> [UGenId]
ugenIds =
    let f u = case u of
                Primitive_U p -> [ugenId p]
                _ -> []
    in ugenFoldr ((++) . f) []

-- | Apply /f/ at 'UId', or no-op at 'NoId'.
atUGenId :: (Int -> Int) -> UGenId -> UGenId
atUGenId f z =
    case z of
      NoId -> NoId
      LinearId -> LinearId
      UId i -> UId (f i)

-- | Add 'idHash' of /e/ to all 'Primitive_U' at /u/.
uprotect :: ID a => a -> UGen -> UGen
uprotect e =
    let e' = idHash e
        f u = case u of
                Primitive_U p -> Primitive_U (p {ugenId = atUGenId (+ e') (ugenId p)})
                _ -> u
    in ugenTraverse f

-- | Variant of 'uprotect' with subsequent identifiers derived by
-- incrementing initial identifier.
uprotect' :: ID a => a -> [UGen] -> [UGen]
uprotect' e =
    let n = map (+ idHash e) [1..]
    in zipWith uprotect n

-- | Make /n/ parallel instances of 'UGen' with protected identifiers.
uclone' :: ID a => a -> Int -> UGen -> [UGen]
uclone' e n = uprotect' e . replicate n

-- | 'mce' variant of 'uclone''.
uclone :: ID a => a -> Int -> UGen -> UGen
uclone e n = mce . uclone' e n

-- | Left to right UGen function composition with 'UGenId' protection.
ucompose :: ID a => a -> [UGen -> UGen] -> UGen -> UGen
ucompose e xs =
    let go [] u = u
        go ((f,k):f') u = go f' (uprotect k (f u))
    in go (zip xs [idHash e ..])

-- | Make /n/ sequential instances of `f' with protected Ids.
useq :: ID a => a -> Int -> (UGen -> UGen) -> UGen -> UGen
useq e n f = ucompose e (replicate n f)
