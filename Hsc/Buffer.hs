module Hsc.Buffer where

import Hsc.UGen
import Hsc.MCE

bufallpass' c buf i dly dcy = mkFilter c [buf,i,dly,dcy] 1 0 r0
bufallpassc = bufallpass' "BufAllpassC"
bufallpassl = bufallpass' "BufAllpassL"
bufallpassn = bufallpass' "BufAllpassN"

bufcomb' c buf i dly dcy = mkFilter c [buf,i,dly,dcy] 1 0 r0
bufcombc = bufcomb' "BufCombC"
bufcombl = bufcomb' "BufCombL"
bufcombn = bufcomb' "BufCombN"

bufdelay' c buf i dly = mkFilter c [buf,i,dly] 1 0 r0
bufdelayc = bufdelay' "BufDelayC"
bufdelayl = bufdelay' "BufDelayL"
bufdelayn = bufdelay' "BufDelayN"

bufchannels   r buf = UGen r "BufChannels"   [buf] [r] 0 r0
bufdur        r buf = UGen r "BufDur"        [buf] [r] 0 r0
bufframes     r buf = UGen r "BufFrames"     [buf] [r] 0 r0
bufratescale  r buf = UGen r "BufRateScale"  [buf] [r] 0 r0
bufsamplerate r buf = UGen r "BufSampleRate" [buf] [r] 0 r0
bufsamples    r buf = UGen r "BufSamples"    [buf] [r] 0 r0

bufrd n r buf phs lp intp = proxyU r "BufRd" [buf,phs,lp,intp] r' 0 r0
    where r' = (replicate n r)

bufwr buf phs lp i = mkFilterMCE "BufWr" [buf,phs,lp,i] 0 0 r0

tgrains n r trg buf rate cntr dur pan amp interp =
    proxyU r "TGrains" [trg,buf,rate,cntr,dur,pan,amp,interp] r' 0 r0
    where r' = (replicate n r)
