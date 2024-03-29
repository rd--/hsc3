-- | Ugen analysis
module Sound.Sc3.Ugen.Analysis where

import Data.List {- base -}

import qualified Sound.Sc3.Common.Mce as Mce {- hsc3 -}
import qualified Sound.Sc3.Common.Rate as Rate {- hsc3 -}
import qualified Sound.Sc3.Ugen.Bindings.Db as Db {- hsc3 -}

import Sound.Sc3.Ugen.Types

{- | Ugen primitive set.
Sees through Proxy and Mrg, possible multiple primitives for Mce.
-}
ugen_primitive_set :: Ugen -> [Primitive Ugen]
ugen_primitive_set u =
  case u of
    Constant_U _ -> []
    Control_U _ -> []
    Label_U _ -> []
    Primitive_U p -> [p]
    Proxy_U p -> [proxySource p]
    Mce_U m -> concatMap ugen_primitive_set (Mce.mce_to_list m)
    Mrg_U m -> ugen_primitive_set (mrgLeft m)

{- | Heuristic based on primitive name (FFT, PV_...).
Note that IFFT is at /control/ rate, not PV_... rate.
-}
primitive_is_pv_rate :: String -> Bool
primitive_is_pv_rate nm = nm == "FFT" || "PV_" `isPrefixOf` nm

-- | Variant on primitive_is_pv_rate.
ugen_is_pv_rate :: Ugen -> Bool
ugen_is_pv_rate = any (primitive_is_pv_rate . ugenName) . ugen_primitive_set

{- | Traverse input graph until an FFT or PV_Split node is encountered, and then locate the buffer input.
Biases left at Mce nodes.

> import Sound.Sc3
> let z = soundIn 4
> let f1 = fft 10 z 0.5 0 1 0
> let f2 = ffta 'a' 1024 z 0.5 0 1 0
> pv_track_buffer (pv_BrickWall f1 0.5) == Right 10
> pv_track_buffer (pv_BrickWall f2 0.5) == Right (localBuf 'a' 1024 1)
-}
pv_track_buffer :: Ugen -> Either String Ugen
pv_track_buffer u =
  case ugen_primitive_set u of
    [] -> Left "pv_track_buffer: not located"
    p : _ -> case ugenName p of
      "FFT" -> Right (ugenInputs p !! 0)
      "PV_Split" -> Right (ugenInputs p !! 1)
      _ -> pv_track_buffer (ugenInputs p !! 0)

{- | Buffer node number of frames. Biases left at Mce nodes.
Sees through LocalBuf, otherwise uses 'bufFrames'.

> buffer_nframes 10 == bufFrames IR 10
> buffer_nframes (control KR "b" 0) == bufFrames KR (control KR "b" 0)
> buffer_nframes (localBuf 'α' 2048 1) == 2048
-}
buffer_nframes :: Ugen -> Ugen
buffer_nframes u =
  case ugen_primitive_set u of
    [] -> Db.bufFrames (rateOf u) u
    p : _ -> case ugenName p of
      "LocalBuf" -> ugenInputs p !! 1
      _ -> Db.bufFrames (rateOf u) u

-- | 'pv_track_buffer' then 'buffer_nframes'.
pv_track_nframes :: Ugen -> Either String Ugen
pv_track_nframes u = pv_track_buffer u >>= Right . buffer_nframes

{- | Ugen is required to be the root node of complete graph.
This function returns the name of the output Ugen (ie. "Out" or an allowed variant) and the input to that Ugen.
It allows multiple-root graphs.
It is in some sense the inverse of 'wrapOut'.
-}
ugen_remove_out_node :: Ugen -> (String, Ugen)
ugen_remove_out_node u =
  let err = error "ugen_remove_out_node?"
      assert_is_output x = if x `elem` ["Out", "ReplaceOut", "OffsetOut"] then x else err
  in case u of
      Primitive_U (Primitive Rate.AudioRate nm (_bus : inputs) [] _special _uid _brk) -> (assert_is_output nm, mce inputs)
      Mrg_U (Mrg lhs rhs) -> let (nm, res) = ugen_remove_out_node lhs in (nm, Mrg_U (Mrg res rhs))
      _ -> err
