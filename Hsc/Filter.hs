module Hsc.Filter where

import Hsc.UGen

allpass' c r i max dly dcy = UGen r c [i, max, dly, dcy] [r] 0 0
allpassn = allpass' "AllpassN"
allpassl = allpass' "AllpassL"
allpassc = allpass' "AllpassC"

bpf  r i freq rq = UGen r "BPF" [i,freq,rq] [r] 0 0
bpz2 r i         = UGen r "BPZ2" [i] [r] 0 0

brf  r i freq rq = UGen r "BRF" [i,freq,rq] [r] 0 0
brz2 r i         = UGen r "BRZ2" [i] [r] 0 0

comb' c r i max dly dcy = UGen r c [i, max, dly, dcy] [r] 0 0
combn = comb' "CombN"
combl = comb' "CombL"
combc = comb' "CombC"

delay1 r i = UGen r "Delay1" [i] [r] 0 0
delay2 r i = UGen r "Delay2" [i] [r] 0 0

delay' c r i max dly = UGen r c [i,max,dly] [r] 0 0
delayc = delay' "DelayC"
delayl = delay' "DelayL"
delayn = delay' "DelayN"

lpf r i f = UGen r "LPF"  [i,f] [r] 0 0
lpz1 r i  = UGen r "LPZ1" [i]   [r] 0 0
lpz2 r i  = UGen r "LPZ2" [i]   [r] 0 0

onepole r i coef = UGen r "OnePole" [i,coef] [r] 0 0
onezero r i coef = UGen r "OneZero" [i,coef] [r] 0 0

rhpf r i freq rq = UGen r "RHPF" [i,freq,rq] [r] 0 0
rlpf r i freq rq = UGen r "RLPF" [i,freq,rq] [r] 0 0

sos r i a0 a1 a2 b1 b2 = UGen r "SOS" [i,a0,a1,a2,b1,b2] [r] 0 0

twopole r i freq radius = UGen r "TwoPole" [i,freq,radius] [r] 0 0
twozero r i freq radius = UGen r "TwoZero" [i,freq,radius] [r] 0 0
