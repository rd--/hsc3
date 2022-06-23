-- | Wavelet unit generators (Nick Collins).
module Sound.Sc3.Ugen.Bindings.Hw.External.Wavelets where

import Sound.Sc3.Common.Rate
import Sound.Sc3.Ugen.Bindings.Hw.Construct
import Sound.Sc3.Ugen.Ugen

-- | Forward wavelet transform.
dwt :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen -> Ugen
dwt buf i h wnt a wns wlt = mkOsc kr "DWT" [buf,i,h,wnt,a,wns,wlt] 1

-- | Inverse of 'dwt'.
idwt :: Ugen -> Ugen -> Ugen -> Ugen -> Ugen
idwt buf wnt wns wlt = mkOsc ar "IDWT" [buf,wnt,wns,wlt] 1

-- | Pass wavelets above a threshold, ie. 'pv_MagAbove'.
wt_MagAbove :: Ugen -> Ugen -> Ugen
wt_MagAbove buf thr = mkOsc kr "WT_MagAbove" [buf,thr] 1

-- | Pass wavelets with /scale/ above threshold.
wt_FilterScale :: Ugen -> Ugen -> Ugen
wt_FilterScale buf wp = mkOsc kr "WT_FilterScale" [buf,wp] 1

-- | Pass wavelets with /time/ above threshold.
wt_TimeWipe :: Ugen -> Ugen -> Ugen
wt_TimeWipe buf wp = mkOsc kr "WT_TimeWipe" [buf,wp] 1

-- | Product in /W/ domain, ie. 'pv_Mul'.
wt_Mul :: Ugen -> Ugen -> Ugen
wt_Mul ba bb = mkOsc kr "WT_Mul" [ba,bb] 1

