module Hsc.UGen.Filter where

import Hsc.Construct (mkFilter, mkFilterMCE)
import Hsc.UGen (UGen, Name)

allpass' c i maxTime dly dcy = mkFilter c [i, maxTime, dly, dcy] 1 0
allpassN = allpass' "AllpassN"
allpassL = allpass' "AllpassL"
allpassC = allpass' "AllpassC"

bpf  i freq rq = mkFilter "BPF" [i,freq,rq] 1 0
bpz2 i         = mkFilter "BPZ2" [i] 1 0

brf  i freq rq = mkFilter "BRF" [i,freq,rq] 1 0
brz2 i         = mkFilter "BRZ2" [i] 1 0

comb' c i maxTime dly dcy = mkFilter c [i, maxTime, dly, dcy] 1 0
combN = comb' "CombN"
combL = comb' "CombL"
combC = comb' "CombC"

decay  i dcy     = mkFilter "Decay"  [i,dcy]     1 0
decay2 i atk dcy = mkFilter "Decay2" [i,atk,dcy] 1 0

delay1 i = mkFilter "Delay1" [i] 1 0
delay2 i = mkFilter "Delay2" [i] 1 0

delay' c i maxTime dly = mkFilter c [i,maxTime,dly] 1 0
delayC = delay' "DelayC"
delayL = delay' "DelayL"
delayN = delay' "DelayN"

formlet i f a d = mkFilter "Formlet" [i,f,a,d] 1 0

fos i a0 a1 b1 = mkFilter "FOS" [i,a0,a1,b1] 1 0

hpf i f = mkFilter "HPF"  [i,f] 1 0
hpz1 i  = mkFilter "HPZ1" [i]   1 0
hpz2 i  = mkFilter "HPZ2" [i]   1 0

klank i fs fp d s = mkFilterMCE "Klank" [i,fs,fp,d] s 1 0

lag   i t = mkFilter "Lag"   [i,t] 1 0
lag2  i t = mkFilter "Lag2"  [i,t] 1 0
lag3  i t = mkFilter "Lag3"  [i,t] 1 0
latch i t = mkFilter "Latch" [i,t] 1 0

leakDC i coef = mkFilter "LeakDC" [i,coef] 1 0

linen g at sl rt da = mkFilter "Linen" [g,at,sl,rt,da] 1 0

linLin i sl sh dl dh = mkFilter "LinLin" [i,sl,sh,dl,dh] 1 0

lpf i f = mkFilter "LPF"  [i,f] 1 0
lpz1 i  = mkFilter "LPZ1" [i]   1 0
lpz2 i  = mkFilter "LPZ2" [i]   1 0

median size i = mkFilter "Median" [size,i] 1 0

mulAdd s m a = mkFilter "MulAdd" [s,m,a] 1 0

onePole i coef = mkFilter "OnePole" [i,coef] 1 0
oneZero i coef = mkFilter "OneZero" [i,coef] 1 0

pitchShift i w p d t = mkFilter "PitchShift" [i,w,p,d,t] 1 0

pulseDivider trig factor start = mkFilter "PulseDivider" [trig, factor, start] 1 0

quad' c' freq a b c xi = mkFilter c' [freq,a,b,c,xi] 1 0
quadC = quad' "QuadC"
quadL = quad' "QuadL"
quadN = quad' "QuadN"

rhpf i freq rq = mkFilter "RHPF" [i,freq,rq] 1 0
rlpf i freq rq = mkFilter "RLPF" [i,freq,rq] 1 0

resonz i freq bwr = mkFilter "Resonz" [i,freq,bwr] 1 0
ringz  i freq dcy = mkFilter "Ringz"  [i,freq,dcy] 1 0

slew i up dn = mkFilter "Slew" [i,up,dn] 1 0

sos i a0 a1 a2 b1 b2 = mkFilter "SOS" [i,a0,a1,a2,b1,b2] 1 0

stepper t r minTime maxTime s v = mkFilter "Stepper" [t,r,minTime,maxTime,s,v] 1 0

twoPole i freq radius = mkFilter "TwoPole" [i,freq,radius] 1 0
twoZero i freq radius = mkFilter "TwoZero" [i,freq,radius] 1 0


allpassC :: UGen -> UGen -> UGen -> UGen -> UGen
allpassL :: UGen -> UGen -> UGen -> UGen -> UGen
allpassN :: UGen -> UGen -> UGen -> UGen -> UGen
allpass' :: Name -> UGen -> UGen -> UGen -> UGen -> UGen
bpf :: UGen -> UGen -> UGen -> UGen
bpz2 :: UGen -> UGen
brf :: UGen -> UGen -> UGen -> UGen
brz2 :: UGen -> UGen
combC :: UGen -> UGen -> UGen -> UGen -> UGen
combL :: UGen -> UGen -> UGen -> UGen -> UGen
combN :: UGen -> UGen -> UGen -> UGen -> UGen
comb' :: Name -> UGen -> UGen -> UGen -> UGen -> UGen
decay :: UGen -> UGen -> UGen
decay2 :: UGen -> UGen -> UGen -> UGen
delay1 :: UGen -> UGen
delay2 :: UGen -> UGen
delayC :: UGen -> UGen -> UGen -> UGen
delayL :: UGen -> UGen -> UGen -> UGen
delayN :: UGen -> UGen -> UGen -> UGen
delay' :: Name -> UGen -> UGen -> UGen -> UGen
formlet :: UGen -> UGen -> UGen -> UGen -> UGen
fos :: UGen -> UGen -> UGen -> UGen -> UGen
hpf :: UGen -> UGen -> UGen
hpz1 :: UGen -> UGen
hpz2 :: UGen -> UGen
klank :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
lag :: UGen -> UGen -> UGen
lag2 :: UGen -> UGen -> UGen
lag3 :: UGen -> UGen -> UGen
latch :: UGen -> UGen -> UGen
leakDC :: UGen -> UGen -> UGen
linLin :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
linen :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
lpf :: UGen -> UGen -> UGen
lpz1 :: UGen -> UGen
lpz2 :: UGen -> UGen
median :: UGen -> UGen -> UGen
mulAdd :: UGen -> UGen -> UGen -> UGen
onePole :: UGen -> UGen -> UGen
oneZero :: UGen -> UGen -> UGen
pitchShift :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pulseDivider :: UGen -> UGen -> UGen -> UGen
quadC :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
quadL :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
quadN :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
quad' :: Name -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
resonz :: UGen -> UGen -> UGen -> UGen
rhpf :: UGen -> UGen -> UGen -> UGen
ringz :: UGen -> UGen -> UGen -> UGen
rlpf :: UGen -> UGen -> UGen -> UGen
slew :: UGen -> UGen -> UGen -> UGen
sos :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
stepper :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
twoPole :: UGen -> UGen -> UGen -> UGen
twoZero :: UGen -> UGen -> UGen -> UGen
