-- | Wavelet unit generators (Nick Collins).
module Sound.SC3.UGen.Bindings.HW.External.Wavelets where

import Sound.SC3.Common.Rate
import Sound.SC3.UGen.Bindings.HW.Construct
import Sound.SC3.UGen.Type

-- | Forward wavelet transform.
dwt :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
dwt buf i h wnt a wns wlt = mkOsc kr "DWT" [buf,i,h,wnt,a,wns,wlt] 1

-- | Inverse of 'dwt'.
idwt :: UGen -> UGen -> UGen -> UGen -> UGen
idwt buf wnt wns wlt = mkOsc ar "IDWT" [buf,wnt,wns,wlt] 1

-- | Pass wavelets above a threshold, ie. 'pv_MagAbove'.
wt_MagAbove :: UGen -> UGen -> UGen
wt_MagAbove buf thr = mkOsc kr "WT_MagAbove" [buf,thr] 1

-- | Pass wavelets with /scale/ above threshold.
wt_FilterScale :: UGen -> UGen -> UGen
wt_FilterScale buf wp = mkOsc kr "WT_FilterScale" [buf,wp] 1

-- | Pass wavelets with /time/ above threshold.
wt_TimeWipe :: UGen -> UGen -> UGen
wt_TimeWipe buf wp = mkOsc kr "WT_TimeWipe" [buf,wp] 1

-- | Product in /W/ domain, ie. 'pv_Mul'.
wt_Mul :: UGen -> UGen -> UGen
wt_Mul ba bb = mkOsc kr "WT_Mul" [ba,bb] 1

