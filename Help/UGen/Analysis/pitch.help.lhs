pitch in initFreq minFreq maxFreq execFreq maxBinsPerOctave median ampThreshold peakThreshold downSample

Autocorrelation pitch follower

This is a better pitch follower than ZeroCrossing, but more costly of
CPU. For most purposes the default settings can be used and only in
needs to be supplied. Pitch returns two values (via an Array of
OutputProxys, see the OutputProxy help file), a freq which is the
pitch estimate and hasFreq, which tells whether a pitch was
found. Some vowels are still problematic, for instance a wide open
mouth sound somewhere between a low pitched short 'a' sound as in
'sat', and long 'i' sound as in 'fire', contains enough overtone
energy to confuse the algorithm.

Default values at sclang are: in = 0, initFreq = 440, minFreq = 60,
maxFreq = 4000, execFreq = 100, maxBinsPerOctave = 16, median = 1,
ampThreshold = 0.01, peakThreshold = 0.5, downSample = 1.

> let x = mouseX KR 220 660 Linear 0.1
>     y = mouseY KR 0.05 0.25 Linear 0.1
>     s = sinOsc AR x 0
>     a = amplitude KR (s * y) 0.05 0.05
>     MCE [f:_] = pitch (s * y) 440 60 4000 100 16 7 0.02 0.5 1
> audition $ MCE [s * y, sinOsc AR (f / 2) 0 * a]

> let s = in' 1 AR numOutputBuses
>     a = amplitude KR s 0.05 0.05
>     MCE [f:_] = pitch s 440 60 4000 100 16 7 0.02 0.5 1
> audition $ MCE [s, sinOsc AR f 0 * a]
