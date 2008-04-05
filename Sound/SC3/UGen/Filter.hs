module Sound.SC3.UGen.Filter where

import Sound.SC3.UGen.UGen
import Sound.SC3.UGen.UGen.Construct
import Data.List

-- | Allpass filter (no interpolation)
allpassN :: UGen -> UGen -> UGen -> UGen -> UGen
allpassN i mt dly dcy = mkFilter "AllpassN" [i, mt, dly, dcy] 1

-- | Allpass filter (linear interpolation)
allpassL :: UGen -> UGen -> UGen -> UGen -> UGen
allpassL i mt dly dcy = mkFilter "AllpassL" [i, mt, dly, dcy] 1

-- | Allpass filter (cubic interpolation)
allpassC :: UGen -> UGen -> UGen -> UGen -> UGen
allpassC i mt dly dcy = mkFilter "AllpassC" [i, mt, dly, dcy] 1

-- | Basic psychoacoustic amplitude compensation.
ampComp :: UGen -> UGen -> UGen -> UGen
ampComp f r e = mkFilter "AmpComp" [f, r, e] 1

-- | ANSI A-weighting curve psychoacoustic amplitude compensation.
ampCompA :: UGen -> UGen -> UGen -> UGen -> UGen
ampCompA f r ma ra = mkFilter "AmpCompA" [f, r, ma, ra] 1

-- | Bandpass filter
bpf :: UGen -> UGen -> UGen -> UGen
bpf i freq rq = mkFilter "BPF" [i, freq, rq] 1

-- | Two zero fixed midpass filter.
bpz2 :: UGen -> UGen
bpz2 i = mkFilter "BPZ2" [i] 1

-- | Band reject filter
brf :: UGen -> UGen -> UGen -> UGen
brf i freq rq = mkFilter "BRF" [i, freq, rq] 1

-- | Two zero fixed midcut filter.
brz2 :: UGen -> UGen
brz2 i = mkFilter "BRZ2" [i] 1

-- | Clip input between lower and upper bounds.
clip :: UGen -> UGen -> UGen -> UGen
clip i l h = mkFilter "Clip" [i, l, h] 1

-- | Comb filter (no interpolation)
combN :: UGen -> UGen -> UGen -> UGen -> UGen
combN i mt dly dcy = mkFilter "CombN" [i, mt, dly, dcy] 1

-- | Comb filter (linear interpolation)
combL :: UGen -> UGen -> UGen -> UGen -> UGen
combL i mt dly dcy = mkFilter "CombL" [i, mt, dly, dcy] 1

-- | Comb filter (cubic interpolation)
combC :: UGen -> UGen -> UGen -> UGen -> UGen
combC i mt dly dcy = mkFilter "CombC" [i, mt, dly, dcy] 1

-- | Convert signal to modal pitch.
degreeToKey :: UGen -> UGen -> UGen -> UGen
degreeToKey b i o = mkFilter "DegreeToKey" [b, i, o] 1

-- | Exponential decay.
decay :: UGen -> UGen -> UGen
decay i dcy = mkFilter "Decay" [i, dcy] 1

-- | Exponential decay (equvalent to $decay dcy - decay atk$).
decay2 :: UGen -> UGen -> UGen -> UGen
decay2 i atk dcy = mkFilter "Decay2" [i, atk, dcy] 1

-- | Single sample delay.
delay1 :: UGen -> UGen
delay1 i = mkFilter "Delay1" [i] 1

-- | Two sample delay.
delay2 :: UGen -> UGen
delay2 i = mkFilter "Delay2" [i] 1

-- | Simple delay line (cubic interpolation).
delayC :: UGen -> UGen -> UGen -> UGen
delayC i mt dly = mkFilter "DelayC" [i, mt, dly] 1

-- | Simple delay line (linear interpolation).
delayL :: UGen -> UGen -> UGen -> UGen
delayL i mt dly = mkFilter "DelayL" [i, mt, dly] 1

-- | Simple delay line (no interpolation).
delayN :: UGen -> UGen -> UGen -> UGen
delayN i mt dly = mkFilter "DelayN" [i, mt, dly] 1

-- | FOF like filter.
formlet :: UGen -> UGen -> UGen -> UGen -> UGen
formlet i f a d = mkFilter "Formlet" [i, f, a, d] 1

-- | First order filter section.
fos :: UGen -> UGen -> UGen -> UGen -> UGen
fos i a0 a1 b1 = mkFilter "FOS" [i, a0, a1, b1] 1

-- | Gate.
gate :: UGen -> UGen -> UGen
gate i t = mkFilter "Gate" [i, t] 1

-- | Hash input values.
hasher :: UGen -> UGen
hasher i = mkFilter "Hasher" [i] 1

-- | Hilbert transform.
hilbert :: UGen -> UGen
hilbert i = mkFilter "Hilbert" [i] 2

-- | Highpass filter.
hpf :: UGen -> UGen -> UGen
hpf i f = mkFilter "HPF" [i, f] 1

-- | Two point difference filter.
hpz1 :: UGen -> UGen
hpz1 i = mkFilter "HPZ1" [i] 1

-- | Two zero fixed highpass filter.
hpz2 :: UGen -> UGen
hpz2 i = mkFilter "HPZ2" [i] 1

-- | Is signal within specified range.
inRange :: UGen -> UGen -> UGen -> UGen
inRange i lo hi = mkFilter "InRange" [i, lo, hi] 1

-- | Fixed resonator filter bank.
klank :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
klank i fs fp d s = mkFilterMCE "Klank" [i, fs, fp, d] s 1

-- | Format frequency, amplitude and decay time data as required for klank.
klankSpec :: [UGen] -> [UGen] -> [UGen] -> UGen
klankSpec f a p = mce ((concat . transpose) [f, a, p])

-- | Simple averaging filter.
lag :: UGen -> UGen -> UGen
lag i t = mkFilter "Lag" [i, t] 1

-- | Nested lag filter.
lag2 :: UGen -> UGen -> UGen
lag2 i t = mkFilter "Lag2" [i, t] 1

-- | Twice nested lag filter.
lag3 :: UGen -> UGen -> UGen
lag3 i t = mkFilter "Lag3" [i, t] 1

-- | Last value before chang above threshhold.
lastValue :: UGen -> UGen -> UGen
lastValue i t = mkFilter "LastValue" [i, t] 1

-- | Sample and hold.
latch :: UGen -> UGen -> UGen
latch i t = mkFilter "Latch" [i, t] 1

-- | Remove DC offset.
leakDC :: UGen -> UGen -> UGen
leakDC i coef = mkFilter "LeakDC" [i, coef] 1

-- | Map from one linear range to another linear range.
linLin :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
linLin i sl sh dl dh = mkFilter "LinLin" [i, sl, sh, dl, dh] 1

-- | Map from a linear range to an exponential range.
linExp :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
linExp i sl sh dl dh = mkFilter "LinExp" [i, sl, sh, dl, dh] 1

-- | Lowpass filter.
lpf :: UGen -> UGen -> UGen
lpf i f = mkFilter "LPF" [i, f] 1

-- | Two point average filter.
lpz1 :: UGen -> UGen
lpz1 i = mkFilter "LPZ1" [i] 1

-- | Two zero fixed lowpass filter.
lpz2 :: UGen -> UGen
lpz2 i = mkFilter "LPZ2" [i] 1

-- | Masks off bits in the mantissa of signal.
mantissaMask :: UGen -> UGen -> UGen
mantissaMask i bits = mkFilter "MantissaMask" [i, bits] 1

-- | Median filter.
median :: UGen -> UGen -> UGen
median size i = mkFilter "Median" [size, i] 1

-- | Moog VCF implementation.
moogFF :: UGen -> UGen -> UGen -> UGen -> UGen
moogFF i f g r = mkFilter "MoogFF" [i, f, g, r] 1

-- | Most changed input.
mostChange :: UGen -> UGen -> UGen
mostChange a b = mkFilter "MostChange" [a, b] 1

-- | Multiply add ternary operator.
mulAdd :: UGen -> UGen -> UGen -> UGen
mulAdd s m a = mkFilter "MulAdd" [s, m, a] 1

-- | Flattens dynamics.
normalizer :: UGen -> UGen -> UGen -> UGen
normalizer i level dur = mkFilter "Normalizer" [i, level, dur] 1

-- | One pole filter.
onePole :: UGen -> UGen -> UGen
onePole i coef = mkFilter "OnePole" [i, coef] 1

-- | One zero filter.
oneZero :: UGen -> UGen -> UGen
oneZero i coef = mkFilter "OneZero" [i, coef] 1

-- | Maximum value.
peak :: UGen -> UGen -> UGen
peak t r = mkFilter "Peak" [t, r] 1

-- | Simple time domain pitch shifter.
pitchShift :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pitchShift i w p d t = mkFilter "PitchShift" [i, w, p, d, t] 1

-- | Karplus-Strong synthesis.
pluck :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pluck i tr mdl dl dc coef = mkFilter "Pluck" [i, tr, mdl, dl, dc, coef] 1

-- | Trigger counter.
pulseCount :: UGen -> UGen -> UGen
pulseCount t r = mkFilter "PulseCount" [t, r] 1

-- | Pass every nth trigger.
pulseDivider :: UGen -> UGen -> UGen -> UGen
pulseDivider t factor start = mkFilter "PulseDivider" [t, factor, start] 1

-- | Resonant highpass filter.
rhpf :: UGen -> UGen -> UGen -> UGen
rhpf i freq rq = mkFilter "RHPF" [i, freq, rq] 1

-- | Resonant lowpass filter.
rlpf :: UGen -> UGen -> UGen -> UGen
rlpf i freq rq = mkFilter "RLPF" [i, freq, rq] 1

-- | Resonant filter.
resonz :: UGen -> UGen -> UGen -> UGen
resonz i freq bwr = mkFilter "Resonz" [i, freq, bwr] 1

-- | Ringing filter (equivalent to Resonz).
ringz :: UGen -> UGen -> UGen -> UGen
ringz i freq dcy = mkFilter "Ringz" [i, freq, dcy] 1

-- | Track maximum level.
runningMax :: UGen -> UGen -> UGen
runningMax i t = mkFilter "RunningMax" [i, t] 1

-- | Track minimum level.
runningMin :: UGen -> UGen -> UGen
runningMin i t = mkFilter "RunningMin" [i, t] 1

-- | Running sum.
runningSum :: UGen -> UGen -> UGen
runningSum i n = mkFilter "RunningSum" [i, n] 1

-- | Select output from array of inputs.
select :: UGen -> UGen -> UGen
select i a = mkFilterMCE "Select" [i] a 1

-- | Send a trigger message from the server back to the all registered clients.
sendTrig :: UGen -> UGen -> UGen -> UGen
sendTrig i k v = mkFilter "SendTrig" [i, k, v] 0

-- | Set-reset flip flop.
setResetFF :: UGen -> UGen -> UGen
setResetFF t r = mkFilter "SetResetFF" [t, r] 1

-- | Wave shaper.
shaper :: UGen -> UGen -> UGen
shaper b s = mkFilter "Shaper" [b, s] 1

-- | Remove transients and higher frequencies.
slew :: UGen -> UGen -> UGen -> UGen
slew i up dn = mkFilter "Slew" [i, up, dn] 1

-- | Second order filter section (biquad). 
sos :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
sos i a0 a1 a2 b1 b2 = mkFilter "SOS" [i, a0, a1, a2, b1, b2] 1

-- | Stepper pulse counter.
stepper :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
stepper t r mn mx s v = mkFilter "Stepper" [t, r, mn, mx, s, v] 1

-- | Triggered linear ramp.
sweep :: UGen -> UGen -> UGen
sweep t r = mkFilter "Sweep" [t, r] 1

-- | Delay trigger by specified interval. 
tDelay :: UGen -> UGen -> UGen
tDelay i d = mkFilter "TDelay" [i, d] 1

-- | Time since last triggered.
timer :: UGen -> UGen
timer t = mkFilter "Timer" [t] 1
 
-- | Toggle flip flop.
toggleFF :: UGen -> UGen
toggleFF t = mkFilter "ToggleFF" [t] 1

-- | When triggered output trigger for specified duration.
trig :: UGen -> UGen -> UGen
trig i d = mkFilter "Trig" [i, d] 1

-- | When triggered output unit signal for specified duration.
trig1 :: UGen -> UGen -> UGen
trig1 i d = mkFilter "Trig1" [i, d] 1

-- | Triggered windex.
twindex :: UGen -> UGen -> UGen -> UGen
twindex i n a = mkFilterMCE "TWindex" [i, n] a 1

-- | Two pole filter.
twoPole :: UGen -> UGen -> UGen -> UGen
twoPole i freq radius = mkFilter "TwoPole" [i, freq, radius] 1

-- | Two zero filter.
twoZero :: UGen -> UGen -> UGen -> UGen
twoZero i freq radius = mkFilter "TwoZero" [i, freq, radius] 1

-- | Index into a table with a signal.
wrapIndex :: UGen -> UGen -> UGen
wrapIndex b i = mkFilter "WrapIndex" [b, i] 1

