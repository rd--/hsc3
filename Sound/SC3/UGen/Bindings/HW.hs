-- | Hand-written bindings.
module Sound.SC3.UGen.Bindings.HW where

import qualified Sound.SC3.UGen.Bindings.HW.Construct as C
import qualified Sound.SC3.UGen.Identifier as I
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.Type
import qualified Sound.SC3.UGen.UGen as U

-- | Zero local buffer.
--
-- ClearBuf does not copy the buffer number through so this is an MRG node.
clearBuf :: UGen -> UGen
clearBuf b = U.mrg2 b (C.mkOsc IR "ClearBuf" [b] 1)

-- | Demand rate weighted random sequence generator.
dwrand :: I.ID i => i -> UGen -> UGen -> UGen -> UGen
dwrand z repeats weights list_ =
    let n = mceDegree_err list_
        weights' = mceExtend n weights
        inp = repeats : constant n : weights'
    in mkUGen Nothing [DR] (Left DR) "Dwrand" inp (Just list_) 1 (Special 0) (U.toUId z)

-- | Outputs signal for @FFT@ chains, without performing FFT.
fftTrigger :: UGen -> UGen -> UGen -> UGen
fftTrigger b h p = C.mkOsc KR "FFTTrigger" [b,h,p] 1

-- | Pack demand-rate FFT bin streams into an FFT chain.
packFFT :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
packFFT b sz from to z mp =
    let n = constant (mceDegree_err mp)
    in C.mkOscMCE KR "PackFFT" [b, sz, from, to, z, n] mp 1

-- | Poll value of input UGen when triggered.
poll :: UGen -> UGen -> UGen -> UGen -> UGen
poll trig_ in_ trigid label_ =
  let q = U.unpackLabel label_
      n = fromIntegral (length q)
  in C.mkFilter "Poll" ([trig_,in_,trigid,n] ++ q) 0

-- | FFT onset detector.
pv_HainsworthFoote :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pv_HainsworthFoote buf h f thr wt = C.mkOsc AR "PV_HainsworthFoote" [buf,h,f,thr,wt] 1

-- | ASCII string to length prefixed list of constant UGens.
--
-- > string_to_ugens "/label" == map fromIntegral [6,47,108,97,98,101,108]
string_to_ugens :: String -> [UGen]
string_to_ugens nm = fromIntegral (length nm) : map (fromIntegral . fromEnum) nm

-- | Send a reply message from the server back to all registered clients.
sendReply :: UGen -> UGen -> String -> [UGen] -> UGen
sendReply i k n v = C.mkFilter "SendReply" ([i,k] ++ string_to_ugens n ++ v) 0

-- | Unpack a single value (magnitude or phase) from an FFT chain
unpack1FFT :: UGen -> UGen -> UGen -> UGen -> UGen
unpack1FFT buf size index' which = C.mkOsc DR "Unpack1FFT" [buf, size, index', which] 1

