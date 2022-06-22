-- | Hand-written bindings.
module Sound.SC3.UGen.Bindings.HW where

import Sound.SC3.Common.Rate
import Sound.SC3.Common.UId
import Sound.SC3.Common.Unsafe

import qualified Sound.SC3.UGen.Bindings.HW.Construct as C
import Sound.SC3.UGen.Types
import qualified Sound.SC3.UGen.UGen as U

-- | Zero local buffer.
--
-- ClearBuf does not copy the buffer number through so this is an Mrg node.
clearBuf :: UGen -> UGen
clearBuf b = U.mrg2 b (C.mkOsc ir "ClearBuf" [b] 1)

-- | Demand rate weighted random sequence generator.
dwrandId :: ID i => i -> UGen -> UGen -> UGen -> UGen
dwrandId z repeats weights list_ =
    let n = mceDegree_err list_
        weights' = mceExtend n weights
        inp = repeats : constant n : weights'
    in mkUGen Nothing [dr] (Left dr) "Dwrand" inp (Just [list_]) 1 (Special 0) (U.toUId z)

dwrandM :: UId m => UGen -> UGen -> UGen -> m UGen
dwrandM = liftUId3 dwrandId

dwrand :: UGen -> UGen -> UGen -> UGen
dwrand = liftUnsafe3 dwrandM

-- | Variant on 'envGen' without enumeration types.
envGen_ll :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
envGen_ll rate gate_ levelScale levelBias timeScale doneAction envelope_ = mkUGen Nothing [kr,ar] (Left rate) "EnvGen" [gate_,levelScale,levelBias,timeScale,doneAction] (Just [envelope_]) 1 (Special 0) NoId

-- | Outputs signal for @FFT@ chains, without performing FFT.
fftTrigger :: UGen -> UGen -> UGen -> UGen
fftTrigger b h p = C.mkOsc kr "FFTTrigger" [b,h,p] 1

-- | Pack demand-rate FFT bin streams into an FFT chain.
packFFT :: UGen -> Int -> Int -> Int -> UGen -> UGen -> UGen
packFFT b sz from to z mp =
    let n = constant (mceDegree_err mp)
    in C.mkOscMCE kr "PackFFT" [b, constant sz, constant from, constant to, z, n] mp 1

-- | Poll value of input UGen when triggered.
poll :: UGen -> UGen -> UGen -> UGen -> UGen
poll trig_ in_ trigid label_ =
  let q = U.unpackLabel True label_
  in C.mkFilter "Poll" ([trig_,in_,trigid] ++ q) 0

-- | FFT onset detector.
pv_HainsworthFoote :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pv_HainsworthFoote buf h f thr wt = C.mkOsc ar "PV_HainsworthFoote" [buf,h,f,thr,wt] 1

-- | FFT feature detector for onset detection.
--
-- buffer, propsc=0.25, prophfe=0.25, prophfc=0.25, propsf=0.25, threshold=1.0, waittime=0.04
pv_JensenAndersen :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pv_JensenAndersen buffer propsc prophfe prophfc propsf threshold waittime = C.mkOsc ar "PV_JensenAndersen" [buffer,propsc,prophfe,prophfc,propsf,threshold,waittime] 1

-- | Ascii string to length prefixed list of constant UGens.
--
-- > string_to_ugens "/label" == map fromIntegral [6,47,108,97,98,101,108]
string_to_ugens :: String -> [UGen]
string_to_ugens nm = fromIntegral (length nm) : map (fromIntegral . fromEnum) nm

-- | Send a reply message from the server back to all registered clients.
sendReply :: UGen -> UGen -> String -> [UGen] -> UGen
sendReply i k n v = C.mkFilter "SendReply" ([i,k] ++ string_to_ugens n ++ v) 0

-- | Unpack a single value (magnitude or phase) from an FFT chain
unpack1FFT :: UGen -> UGen -> UGen -> UGen -> UGen
unpack1FFT buf size index_ which = C.mkOsc dr "Unpack1FFT" [buf, size, index_, which] 1

