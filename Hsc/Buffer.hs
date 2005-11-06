module Hsc.Buffer where

import Hsc.UGen
import Hsc.MCE

bufallpass' c r buf i dly dcy = UGen r c [buf,i,dly,dcy] [r] 0 0
bufallpassc = bufallpass' "BufAllpassC"
bufallpassl = bufallpass' "BufAllpassL"
bufallpassn = bufallpass' "BufAllpassN"

bufcomb' c r buf i dly dcy = UGen r c [buf,i,dly,dcy] [r] 0 0
bufcombc = bufcomb' "BufCombC"
bufcombl = bufcomb' "BufCombL"
bufcombn = bufcomb' "BufCombN"

bufdelay' c r buf i dly = UGen r c [buf,i,dly] [r] 0 0
bufdelayc = bufdelay' "BufDelayC"
bufdelayl = bufdelay' "BufDelayL"
bufdelayn = bufdelay' "BufDelayN"

bufchannels   r buf = UGen r "BufChannels"   [buf] [r] 0 0
bufdur        r buf = UGen r "BufDur"        [buf] [r] 0 0
bufframes     r buf = UGen r "BufFrames"     [buf] [r] 0 0
bufratescale  r buf = UGen r "BufRateScale"  [buf] [r] 0 0
bufsamplerate r buf = UGen r "BufSampleRate" [buf] [r] 0 0
bufsamples    r buf = UGen r "BufSamples"    [buf] [r] 0 0

bufrd n r buf phs lp intp = proxyU r "BufRd" [buf,phs,lp,intp] r' 0 0
    where r' = (replicate n r)
bufrd1 = bufrd 1
bufrd2 = bufrd 2
bufrd4 = bufrd 4
bufrd8 = bufrd 8

bufwr r buf phs lp i = UGen r "BufWr" ([buf,phs,lp] ++ forceMCE i) [] 0 0

tgrains n r trg buf rate cntr dur pan amp interp = 
    proxyU r "TGrains" [trg,buf,rate,cntr,dur,pan,amp,interp] r' 0 0
    where r' = (replicate n r)
tgrains1 = tgrains 1
tgrains2 = tgrains 2
tgrains4 = tgrains 4
tgrains8 = tgrains 8
