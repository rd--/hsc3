module Hsc.Buffer where

import Hsc.Construct

bufallpass' c buf i dly dcy = mkFilter c [buf,i,dly,dcy] 1 0
bufallpassc = bufallpass' "BufAllpassC"
bufallpassl = bufallpass' "BufAllpassL"
bufallpassn = bufallpass' "BufAllpassN"

bufcomb' c buf i dly dcy = mkFilter c [buf,i,dly,dcy] 1 0
bufcombc = bufcomb' "BufCombC"
bufcombl = bufcomb' "BufCombL"
bufcombn = bufcomb' "BufCombN"

bufdelay' c buf i dly = mkFilter c [buf,i,dly] 1 0
bufdelayc = bufdelay' "BufDelayC"
bufdelayl = bufdelay' "BufDelayL"
bufdelayn = bufdelay' "BufDelayN"

bufchannels   r buf = mkOsc r "BufChannels"   [buf] 1 0
bufdur        r buf = mkOsc r "BufDur"        [buf] 1 0
bufframes     r buf = mkOsc r "BufFrames"     [buf] 1 0
bufratescale  r buf = mkOsc r "BufRateScale"  [buf] 1 0
bufsamplerate r buf = mkOsc r "BufSampleRate" [buf] 1 0
bufsamples    r buf = mkOsc r "BufSamples"    [buf] 1 0

bufrd n r buf phs lp intp = mkOsc r "BufRd" [buf,phs,lp,intp] n 0
bufwr buf phs lp i = mkFilterMCE "BufWr" [buf,phs,lp] i 0 0

tgrains n r trg buf rate cntr dur pan amp interp = mkOsc r "TGrains" i n 0
    where i = [trg,buf,rate,cntr,dur,pan,amp,interp]
