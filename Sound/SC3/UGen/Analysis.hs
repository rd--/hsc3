-- | UGen analysis
module Sound.SC3.UGen.Analysis where

import Data.List {- base -}

import qualified Sound.SC3.UGen.Bindings.DB as DB
import qualified Sound.SC3.UGen.MCE as MCE
import Sound.SC3.UGen.Type

-- | UGen primitive.  Sees through Proxy and MRG, possible multiple
-- primitives for MCE.
ugen_primitive :: UGen -> [Primitive]
ugen_primitive u =
    case u of
      Constant_U _ -> []
      Control_U _ -> []
      Label_U _ -> []
      Primitive_U p -> [p]
      Proxy_U p -> [proxySource p]
      MCE_U m -> concatMap ugen_primitive (MCE.mce_elem m)
      MRG_U m -> ugen_primitive (mrgLeft m)

-- | Heuristic based on primitive name (@FFT@, @PV_@).  Note that
-- @IFFT@ is at /control/ rate, not @PV@ rate.
primitive_is_pv_rate :: String -> Bool
primitive_is_pv_rate nm = nm == "FFT" || "PV_" `isPrefixOf` nm

-- | Variant on primitive_is_pv_rate.
ugen_is_pv_rate :: UGen -> Bool
ugen_is_pv_rate = any (primitive_is_pv_rate . ugenName) . ugen_primitive

-- | Traverse input graph until an @FFT@ or @PV_Split@ node is
-- encountered, and then locate the buffer input.  Biases left at MCE
-- nodes.
--
-- > import Sound.SC3
-- > let z = soundIn 4
-- > let f1 = fft 10 z 0.5 0 1 0
-- > let f2 = ffta 'a' 1024 z 0.5 0 1 0
-- > pv_track_buffer (pv_BrickWall f1 0.5) == Right 10
-- > pv_track_buffer (pv_BrickWall f2 0.5) == Right (localBuf 'a' 1024 1)
pv_track_buffer :: UGen -> Either String UGen
pv_track_buffer u =
    case ugen_primitive u of
      [] -> Left "pv_track_buffer: not located"
      p:_ -> case ugenName p of
               "FFT" -> Right (ugenInputs p !! 0)
               "PV_Split" -> Right (ugenInputs p !! 1)
               _ -> pv_track_buffer (ugenInputs p !! 0)

-- | Buffer node number of frames. Biases left at MCE nodes.  Sees
-- through @LocalBuf@, otherwise uses 'bufFrames'.
--
-- > buffer_nframes 10 == bufFrames IR 10
-- > buffer_nframes (control KR "b" 0) == bufFrames KR (control KR "b" 0)
-- > buffer_nframes (localBuf 'α' 2048 1) == 2048
buffer_nframes :: UGen -> UGen
buffer_nframes u =
    case ugen_primitive u of
      [] -> DB.bufFrames (rateOf u) u
      p:_ -> case ugenName p of
               "LocalBuf" -> ugenInputs p !! 1
               _ -> DB.bufFrames (rateOf u) u

-- | 'pv_track_buffer' then 'buffer_nframes'.
pv_track_nframes :: UGen -> Either String UGen
pv_track_nframes u = pv_track_buffer u >>= Right . buffer_nframes
