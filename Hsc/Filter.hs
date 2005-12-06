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

clip r i l h = UGen r "Clip" [i,l,h] [r] 0 0

comb' c r i max dly dcy = UGen r c [i, max, dly, dcy] [r] 0 0
combn = comb' "CombN"
combl = comb' "CombL"
combc = comb' "CombC"

decay  r i dcy     = UGen r "Decay"  [i,dcy]     [r] 0 0
decay2 r i atk dcy = UGen r "Decay2" [i,atk,dcy] [r] 0 0

delay1 r i = UGen r "Delay1" [i] [r] 0 0
delay2 r i = UGen r "Delay2" [i] [r] 0 0

delay' c r i max dly = UGen r c [i,max,dly] [r] 0 0
delayc = delay' "DelayC"
delayl = delay' "DelayL"
delayn = delay' "DelayN"

formlet r i f a d = UGen r "Formlet" [i,f,a,d] [r] 0 0

fos r i a0 a1 b1 = UGen r "FOS" [i,a0,a1,b1] [r] 0 0

hpf r i f = UGen r "HPF"  [i,f] [r] 0 0
hpz1 r i  = UGen r "HPZ1" [i]   [r] 0 0
hpz2 r i  = UGen r "HPZ2" [i]   [r] 0 0

lag  r i t = UGen r "Lag"  [i,t] [r] 0 0
lag2 r i t = UGen r "Lag2" [i,t] [r] 0 0
lag3 r i t = UGen r "Lag3" [i,t] [r] 0 0

leakdc r i coef = UGen r "LeakDC" [i,coef] [r] 0 0

linlin r i sl sh dl dh = UGen r "LinLin" [i,sl,sh,dl,dh] [r] 0 0

lpf r i f = UGen r "LPF"  [i,f] [r] 0 0
lpz1 r i  = UGen r "LPZ1" [i]   [r] 0 0
lpz2 r i  = UGen r "LPZ2" [i]   [r] 0 0

median r length i = UGen r "Median" [length,i] [r] 0 0

onepole r i coef = UGen r "OnePole" [i,coef] [r] 0 0
onezero r i coef = UGen r "OneZero" [i,coef] [r] 0 0

pitchshift r i w p d t = UGen r "PitchShift" [i,w,p,d,t] [r] 0 0

quad' c' r freq a b c xi = UGen r c' [freq,a,b,c,xi] [r] 0 0
quadc = quad' "QuadC"
quadl = quad' "QuadL"
quadn = quad' "QuadN"

rhpf r i freq rq = UGen r "RHPF" [i,freq,rq] [r] 0 0
rlpf r i freq rq = UGen r "RLPF" [i,freq,rq] [r] 0 0

resonz r i freq bwr = UGen r "Resonz" [i,freq,bwr] [r] 0 0
ringz  r i freq dcy = UGen r "Ringz"  [i,freq,dcy] [r] 0 0

slew r i up dn = UGen r "Slew" [i,up,dn] [r] 0 0

sos r i a0 a1 a2 b1 b2 = UGen r "SOS" [i,a0,a1,a2,b1,b2] [r] 0 0

twopole r i freq radius = UGen r "TwoPole" [i,freq,radius] [r] 0 0
twozero r i freq radius = UGen r "TwoZero" [i,freq,radius] [r] 0 0
