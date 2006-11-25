module Sound.SC3.UGen.Filter where

import Sound.SC3.UGen.UGen (UGen, Name, mkFilter, mkFilterMCE)

mkAllpass :: Name -> UGen -> UGen -> UGen -> UGen -> UGen
mkAllpass c i maxTime dly dcy = mkFilter c [i, maxTime, dly, dcy] 1 0

-- | Allpass filter (no interpolation)
allpassN :: UGen -> UGen -> UGen -> UGen -> UGen
allpassN = mkAllpass "AllpassN"

-- | Allpass filter (linear interpolation)
allpassL :: UGen -> UGen -> UGen -> UGen -> UGen
allpassL = mkAllpass "AllpassL"

-- | Allpass filter (cubic interpolation)
allpassC :: UGen -> UGen -> UGen -> UGen -> UGen
allpassC = mkAllpass "AllpassC"

-- | Bandpass filter
bpf :: UGen -> UGen -> UGen -> UGen
bpf i freq rq = mkFilter "BPF" [i,freq,rq] 1 0

-- | Two zero fixed midpass filter.
bpz2 :: UGen -> UGen
bpz2 i = mkFilter "BPZ2" [i] 1 0

-- | Band reject filter
brf :: UGen -> UGen -> UGen -> UGen
brf i freq rq = mkFilter "BRF" [i,freq,rq] 1 0

-- | Two zero fixed midcut filter.
brz2 :: UGen -> UGen
brz2 i = mkFilter "BRZ2" [i] 1 0

mkComb :: Name -> UGen -> UGen -> UGen -> UGen -> UGen
mkComb c i maxTime dly dcy = mkFilter c [i, maxTime, dly, dcy] 1 0

-- | Comb filter (no interpolation)
combN :: UGen -> UGen -> UGen -> UGen -> UGen
combN = mkComb "CombN"

-- | Comb filter (linear interpolation)
combL :: UGen -> UGen -> UGen -> UGen -> UGen
combL = mkComb "CombL"

-- | Comb filter (cubic interpolation)
combC :: UGen -> UGen -> UGen -> UGen -> UGen
combC = mkComb "CombC"

-- | Exponential decay.
decay :: UGen -> UGen -> UGen
decay i dcy = mkFilter "Decay" [i,dcy] 1 0

-- | Exponential decay (equvalent to $decay dcy - decay atk$).
decay2 :: UGen -> UGen -> UGen -> UGen
decay2 i atk dcy = mkFilter "Decay2" [i,atk,dcy] 1 0

-- | Single sample delay.
delay1 :: UGen -> UGen
delay1 i = mkFilter "Delay1" [i] 1 0

-- | Two sample delay.
delay2 :: UGen -> UGen
delay2 i = mkFilter "Delay2" [i] 1 0

mkDelay :: Name -> UGen -> UGen -> UGen -> UGen
mkDelay c i maxTime dly = mkFilter c [i,maxTime,dly] 1 0

-- | Simple delay line (cubic interpolation).
delayC :: UGen -> UGen -> UGen -> UGen
delayC = mkDelay "DelayC"

-- | Simple delay line (linear interpolation).
delayL :: UGen -> UGen -> UGen -> UGen
delayL = mkDelay "DelayL"

-- | Simple delay line (no interpolation).
delayN :: UGen -> UGen -> UGen -> UGen
delayN = mkDelay "DelayN"

-- | FOF like filter.
formlet :: UGen -> UGen -> UGen -> UGen -> UGen
formlet i f a d = mkFilter "Formlet" [i,f,a,d] 1 0

-- | First order filter section.
fos :: UGen -> UGen -> UGen -> UGen -> UGen
fos i a0 a1 b1 = mkFilter "FOS" [i,a0,a1,b1] 1 0

-- | Highpass filter.
hpf :: UGen -> UGen -> UGen
hpf i f = mkFilter "HPF" [i,f] 1 0

-- | Two point difference filter.
hpz1 :: UGen -> UGen
hpz1 i = mkFilter "HPZ1" [i] 1 0

-- | Two zero fixed highpass filter.
hpz2 :: UGen -> UGen
hpz2 i = mkFilter "HPZ2" [i] 1 0

klank :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
klank i fs fp d s = mkFilterMCE "Klank" [i,fs,fp,d] s 1 0

-- | Simple averaging filter.
lag :: UGen -> UGen -> UGen
lag i t = mkFilter "Lag" [i,t] 1 0

lag2 :: UGen -> UGen -> UGen
lag2 i t = mkFilter "Lag2" [i,t] 1 0

lag3 :: UGen -> UGen -> UGen
lag3 i t = mkFilter "Lag3" [i,t] 1 0

-- | Sample and hold.
latch :: UGen -> UGen -> UGen
latch i t = mkFilter "Latch" [i,t] 1 0

-- | Remove DC offset.
leakDC :: UGen -> UGen -> UGen
leakDC i coef = mkFilter "LeakDC" [i,coef] 1 0

-- | Linear envelope generator.
linen :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
linen g at sl rt da = mkFilter "Linen" [g,at,sl,rt,da] 1 0

-- | Map from one linear range to another linear range.
linLin :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
linLin i sl sh dl dh = mkFilter "LinLin" [i,sl,sh,dl,dh] 1 0

-- | Lowpass filter.
lpf :: UGen -> UGen -> UGen
lpf i f = mkFilter "LPF" [i,f] 1 0

-- | Two point average filter.
lpz1 :: UGen -> UGen
lpz1 i = mkFilter "LPZ1" [i] 1 0

-- | Two zero fixed lowpass filter.
lpz2 :: UGen -> UGen
lpz2 i = mkFilter "LPZ2" [i] 1 0

-- | Median filter.
median :: UGen -> UGen -> UGen
median size i = mkFilter "Median" [size,i] 1 0

-- | Multiply add ternary operator.
mulAdd :: UGen -> UGen -> UGen -> UGen
mulAdd s m a = mkFilter "MulAdd" [s,m,a] 1 0

-- | Flattens dynamics.
normalizer :: UGen -> UGen -> UGen -> UGen
normalizer i level dur = mkFilter "Normalizer" [i,level,dur] 1 0

-- | One pole filter.
onePole :: UGen -> UGen -> UGen
onePole i coef = mkFilter "OnePole" [i,coef] 1 0

-- | One zero filter.
oneZero :: UGen -> UGen -> UGen
oneZero i coef = mkFilter "OneZero" [i,coef] 1 0

pitchShift :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pitchShift i w p d t = mkFilter "PitchShift" [i,w,p,d,t] 1 0

pulseDivider :: UGen -> UGen -> UGen -> UGen
pulseDivider trig factor start = mkFilter "PulseDivider" [trig, factor, start] 1 0

mkQuad :: Name -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
mkQuad c' freq a b c xi = mkFilter c' [freq,a,b,c,xi] 1 0

quadC :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
quadC = mkQuad "QuadC"

quadL :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
quadL = mkQuad "QuadL"

quadN :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
quadN = mkQuad "QuadN"

-- | Resonant highpass filter.
rhpf :: UGen -> UGen -> UGen -> UGen
rhpf i freq rq = mkFilter "RHPF" [i,freq,rq] 1 0

-- | Resonant lowpass filter.
rlpf :: UGen -> UGen -> UGen -> UGen
rlpf i freq rq = mkFilter "RLPF" [i,freq,rq] 1 0

-- | Resonant filter.
resonz :: UGen -> UGen -> UGen -> UGen
resonz i freq bwr = mkFilter "Resonz" [i,freq,bwr] 1 0

-- | Ringing filter (equivalent to Resonz).
ringz :: UGen -> UGen -> UGen -> UGen
ringz i freq dcy = mkFilter "Ringz" [i,freq,dcy] 1 0

slew :: UGen -> UGen -> UGen -> UGen
slew i up dn = mkFilter "Slew" [i,up,dn] 1 0

-- | Second order filter section (biquad). 
sos :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
sos i a0 a1 a2 b1 b2 = mkFilter "SOS" [i,a0,a1,a2,b1,b2] 1 0

stepper :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
stepper t r minTime maxTime s v = mkFilter "Stepper" [t,r,minTime,maxTime,s,v] 1 0

-- | Two pole filter.
twoPole :: UGen -> UGen -> UGen -> UGen
twoPole i freq radius = mkFilter "TwoPole" [i,freq,radius] 1 0

-- | Two zero filter.
twoZero :: UGen -> UGen -> UGen -> UGen
twoZero i freq radius = mkFilter "TwoZero" [i,freq,radius] 1 0
