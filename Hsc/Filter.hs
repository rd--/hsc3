module Hsc.Filter where

import Hsc.Construct (mkFilter)

allpass' c i max dly dcy = mkFilter c [i, max, dly, dcy] 1 0
allpassn = allpass' "AllpassN"
allpassl = allpass' "AllpassL"
allpassc = allpass' "AllpassC"

bpf  i freq rq = mkFilter "BPF" [i,freq,rq] 1 0
bpz2 i         = mkFilter "BPZ2" [i] 1 0

brf  i freq rq = mkFilter "BRF" [i,freq,rq] 1 0
brz2 i         = mkFilter "BRZ2" [i] 1 0

comb' c i max dly dcy = mkFilter c [i, max, dly, dcy] 1 0
combn = comb' "CombN"
combl = comb' "CombL"
combc = comb' "CombC"

decay  i dcy     = mkFilter "Decay"  [i,dcy]     1 0
decay2 i atk dcy = mkFilter "Decay2" [i,atk,dcy] 1 0

delay1 i = mkFilter "Delay1" [i] 1 0
delay2 i = mkFilter "Delay2" [i] 1 0

delay' c i max dly = mkFilter c [i,max,dly] 1 0
delayc = delay' "DelayC"
delayl = delay' "DelayL"
delayn = delay' "DelayN"

formlet i f a d = mkFilter "Formlet" [i,f,a,d] 1 0

fos i a0 a1 b1 = mkFilter "FOS" [i,a0,a1,b1] 1 0

hpf i f = mkFilter "HPF"  [i,f] 1 0
hpz1 i  = mkFilter "HPZ1" [i]   1 0
hpz2 i  = mkFilter "HPZ2" [i]   1 0

lag   i t = mkFilter "Lag"   [i,t] 1 0
lag2  i t = mkFilter "Lag2"  [i,t] 1 0
lag3  i t = mkFilter "Lag3"  [i,t] 1 0
latch i t = mkFilter "Latch" [i,t] 1 0

leakdc i coef = mkFilter "LeakDC" [i,coef] 1 0

linlin i sl sh dl dh = mkFilter "LinLin" [i,sl,sh,dl,dh] 1 0

lpf i f = mkFilter "LPF"  [i,f] 1 0
lpz1 i  = mkFilter "LPZ1" [i]   1 0
lpz2 i  = mkFilter "LPZ2" [i]   1 0

median length i = mkFilter "Median" [length,i] 1 0

onepole i coef = mkFilter "OnePole" [i,coef] 1 0
onezero i coef = mkFilter "OneZero" [i,coef] 1 0

pitchshift i w p d t = mkFilter "PitchShift" [i,w,p,d,t] 1 0

quad' c' freq a b c xi = mkFilter c' [freq,a,b,c,xi] 1 0
quadc = quad' "QuadC"
quadl = quad' "QuadL"
quadn = quad' "QuadN"

rhpf i freq rq = mkFilter "RHPF" [i,freq,rq] 1 0
rlpf i freq rq = mkFilter "RLPF" [i,freq,rq] 1 0

resonz i freq bwr = mkFilter "Resonz" [i,freq,bwr] 1 0
ringz  i freq dcy = mkFilter "Ringz"  [i,freq,dcy] 1 0

slew i up dn = mkFilter "Slew" [i,up,dn] 1 0

sos i a0 a1 a2 b1 b2 = mkFilter "SOS" [i,a0,a1,a2,b1,b2] 1 0

twopole i freq radius = mkFilter "TwoPole" [i,freq,radius] 1 0
twozero i freq radius = mkFilter "TwoZero" [i,freq,radius] 1 0
