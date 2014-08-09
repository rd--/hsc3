-- | Hand-written bindings.
module Sound.SC3.UGen.Bindings.HW where

import Sound.SC3.UGen.Bindings.HW.Construct
import Sound.SC3.UGen.Identifier
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UGen

-- | Demand rate weighted random sequence generator.
dwrand :: ID i => i -> UGen -> UGen -> UGen -> UGen
dwrand z repeats weights list_ =
    let n = mceDegree list_
        weights' = mceExtend n weights
        inp = repeats : constant n : weights'
    in mkUGen Nothing [DR] (Left DR) "Dwrand" inp (Just list_) 1 (Special 0) (toUId z)

-- | Outputs signal for @FFT@ chains, without performing FFT.
fftTrigger :: UGen -> UGen -> UGen -> UGen
fftTrigger b h p = mkOsc KR "FFTTrigger" [b,h,p] 1

-- | Pack demand-rate FFT bin streams into an FFT chain.
packFFT :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
packFFT b sz from to z mp =
    let n = constant (mceDegree mp)
    in mkOscMCE KR "PackFFT" [b, sz, from, to, z, n] mp 1

-- | Poll value of input UGen when triggered.
poll :: UGen -> UGen -> UGen -> UGen -> UGen
poll t i l tr = mkFilter "Poll" ([t,i,tr] ++ unpackLabel l) 0

-- | Send a reply message from the server back to the all registered clients.
sendReply :: UGen -> UGen -> String -> [UGen] -> UGen
sendReply i k n v =
    let n' = map (fromIntegral . fromEnum) n
        s = fromIntegral (length n')
    in mkFilter "SendReply" ([i,k,s] ++ n' ++ v) 0

-- | Set local buffer values.
setBuf :: UGen -> [UGen] -> UGen -> UGen
setBuf b xs o =
    let i = [b, o, fromIntegral (length xs)] ++ xs
    in mkUGen Nothing [IR] (Left IR) "SetBuf" i Nothing 1 (Special 0) NoId

-- | Unpack a single value (magnitude or phase) from an FFT chain
unpack1FFT :: UGen -> UGen -> UGen -> UGen -> UGen
unpack1FFT buf size index' which = mkOsc DR "Unpack1FFT" [buf, size, index', which] 1

