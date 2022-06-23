-- | Hand-written bindings.
module Sound.Sc3.Ugen.Bindings.Hw where

import Sound.Sc3.Common.Rate
import Sound.Sc3.Common.Uid
import Sound.Sc3.Common.Unsafe

import qualified Sound.Sc3.Ugen.Bindings.Hw.Construct as C
import Sound.Sc3.Ugen.Types
import qualified Sound.Sc3.Ugen.Util as Util

-- | Zero local buffer.
--
-- ClearBuf does not copy the buffer number through so this is an Mrg node.
clearBuf :: Ugen -> Ugen
clearBuf b = Util.mrg2 b (C.mkOsc ir "ClearBuf" [b] 1)

-- | Demand rate weighted random sequence generator.
dwrandId :: ID i => i -> Ugen -> Ugen -> Ugen -> Ugen
dwrandId z repeats weights list_ =
    let n = mceDegree_err list_
        weights' = mceExtend n weights
        inp = repeats : constant n : weights'
    in mkUgen Nothing [dr] (Left dr) "Dwrand" inp (Just [list_]) 1 (Special 0) (Util.toUid z)

dwrandM :: Uid m => Ugen -> Ugen -> Ugen -> m Ugen
dwrandM = liftUid3 dwrandId

dwrand :: Ugen -> Ugen -> Ugen -> Ugen
dwrand = liftUnsafe3 dwrandM

-- | Variant on 'envGen' without enumeration types.
envGen_ll :: Rate -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
envGen_ll rate gate_ levelScale levelBias timeScale doneAction envelope_ = mkUgen Nothing [kr,ar] (Left rate) "EnvGen" [gate_,levelScale,levelBias,timeScale,doneAction] (Just [envelope_]) 1 (Special 0) NoId

-- | Outputs signal for @FFT@ chains, without performing FFT.
fftTrigger :: Ugen -> Ugen -> Ugen -> Ugen
fftTrigger b h p = C.mkOsc kr "FFTTrigger" [b,h,p] 1

-- | Pack demand-rate FFT bin streams into an FFT chain.
packFFT :: Ugen -> Int -> Int -> Int -> Ugen -> Ugen -> Ugen
packFFT b sz from to z mp =
    let n = constant (mceDegree_err mp)
    in C.mkOscMCE kr "PackFFT" [b, constant sz, constant from, constant to, z, n] mp 1

-- | Poll value of input Ugen when triggered.
poll :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen
poll trig_ in_ trigid label_ =
  let q = Util.unpackLabel True label_
  in C.mkFilter "Poll" ([trig_,in_,trigid] ++ q) 0

-- | FFT onset detector.
pv_HainsworthFoote :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
pv_HainsworthFoote buf h f thr wt = C.mkOsc ar "PV_HainsworthFoote" [buf,h,f,thr,wt] 1

-- | FFT feature detector for onset detection.
--
-- buffer, propsc=0.25, prophfe=0.25, prophfc=0.25, propsf=0.25, threshold=1.0, waittime=0.04
pv_JensenAndersen :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
pv_JensenAndersen buffer propsc prophfe prophfc propsf threshold waittime = C.mkOsc ar "PV_JensenAndersen" [buffer,propsc,prophfe,prophfc,propsf,threshold,waittime] 1

-- | Ascii string to length prefixed list of constant Ugens.
--
-- > string_to_ugens "/label" == map fromIntegral [6,47,108,97,98,101,108]
string_to_ugens :: String -> [Ugen]
string_to_ugens nm = fromIntegral (length nm) : map (fromIntegral . fromEnum) nm

-- | Send a reply message from the server back to all registered clients.
sendReply :: Ugen -> Ugen -> String -> [Ugen] -> Ugen
sendReply i k n v = C.mkFilter "SendReply" ([i,k] ++ string_to_ugens n ++ v) 0

-- | Unpack a single value (magnitude or phase) from an FFT chain
unpack1FFT :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen
unpack1FFT buf size index_ which = C.mkOsc dr "Unpack1FFT" [buf, size, index_, which] 1

