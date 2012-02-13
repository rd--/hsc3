-- | Envelope generators.
module Sound.SC3.UGen.Envelope where

import Data.Maybe
import Sound.SC3.UGen.Enum
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.UGen
import Sound.SC3.UGen.Utilities

-- | SC3 envelope segment model
data Envelope a =
    Envelope {env_levels :: [a] -- ^ Set of /n/ levels
             ,env_times :: [a] -- ^ Set of /n-1/ time intervals
             ,env_curves :: [Envelope_Curve a] -- ^ Possibly empty curve set
             ,env_release_node :: Maybe Int -- ^ Maybe index to release node
             ,env_loop_node :: Maybe Int} -- ^ Maybe index to loop node

-- | Linear SC3 form of 'Envelope' data.
envelope_sc3_array :: Num a => Envelope a -> Maybe [a]
envelope_sc3_array (Envelope l t c rn ln) =
    let n = length t
        n' = fromIntegral n
        rn' = fromIntegral (fromMaybe (-99) rn)
        ln' = fromIntegral (fromMaybe (-99) ln)
        c' = if null c then replicate n EnvLin else take n (cycle c)
        f i j k = [i,j,env_curve_shape k,env_curve_value k]
    in case l of
         l0:l' -> Just (l0 : n' : rn' : ln' : concat (zipWith3 f l' t c'))
         _ -> Nothing

-- | Segment based envelope generator.
envGen :: Rate -> UGen -> UGen -> UGen -> UGen -> DoneAction -> Envelope UGen -> UGen
envGen r gate lvl bias scale act e =
    let err = error "envGen: bad Envelope"
        z = fromMaybe err (envelope_sc3_array e)
        i = [gate, lvl, bias, scale, from_done_action act] ++ z
    in mkOsc r "EnvGen" i 1

-- | Line generator.
line :: Rate -> UGen -> UGen -> UGen -> DoneAction -> UGen
line r start end dur act = mkOsc r "Line" [start, end, dur, from_done_action act] 1

-- | Exponential line generator.
xLine :: Rate -> UGen -> UGen -> UGen -> DoneAction -> UGen
xLine r start end dur act = mkOsc r "XLine" [start, end, dur, from_done_action act] 1

-- | Free node on trigger.
freeSelf :: UGen -> UGen
freeSelf i = mkFilter "FreeSelf" [i] 1

-- | Free node on done action at source.
freeSelfWhenDone :: UGen -> UGen
freeSelfWhenDone i = mkFilter "FreeSelfWhenDone" [i] 1

-- | Pause specified node on trigger.
pause :: UGen -> UGen -> UGen
pause t n = mkFilter "Pause" [t, n] 1

-- | Pause node on trigger.
pauseSelf :: UGen -> UGen
pauseSelf i = mkFilter "PauseSelf" [i] 1

-- | Pause node on done action at source.
pauseSelfWhenDone :: UGen -> UGen
pauseSelfWhenDone i = mkFilter "PauseSelfWhenDone" [i] 1

-- | One while the source is marked done, else zero.
done :: UGen -> UGen
done i = mkFilter "Done" [i] 1

-- | Raise specified done action when input goes silent.
detectSilence ::  UGen -> UGen -> UGen -> DoneAction -> UGen
detectSilence i a t act = mkFilter "DetectSilence" [i, a, t, from_done_action act] 1

-- | When triggered free specified node.
free :: UGen -> UGen -> UGen
free i n = mkFilter "Free" [i, n] 1

-- | Linear envelope generator.
linen :: UGen -> UGen -> UGen -> UGen -> DoneAction -> UGen
linen g at sl rt da = mkFilter "Linen" [g, at, sl, rt, from_done_action da] 1
