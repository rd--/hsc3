lfdClipNoise rate freq

Like LFClipNoise, it generates the values -1 or +1 at a rate given
by the freq argument, with two differences: no time quantization,
and fast recovery from low freq values.

LFClipNoise, as well as LFNoise0,1,2 quantize to the nearest
integer division of the samplerate, and they poll the freq argument
only when scheduled, and thus seem to hang when freqs get very
low.

If you don't need very high or very low freqs, or use fixed freqs,
LFClipNoise is more efficient.

Try wiggling mouse quickly; lfClipNoise frequently seems stuck,
lfdClipNoise changes smoothly.

> import Sound.SC3.Monadic

> let x = mouseX KR 0.1 1000 Exponential 0.2
> in do { n <- lfdClipNoise AR x
>       ; audition (out 0 (sinOsc AR (n * 200 + 500) 0 * 0.05)) }

> let x = mouseX KR 0.1 1000 Exponential 0.2
> in do { n <- lfClipNoise AR x
>       ; audition (out 0 (sinOsc AR (n * 200 + 500) 0 * 0.05)) }

lfClipNoise quantizes time steps at high freqs, lfdClipNoise does not:

> let f = xLine KR 1000 20000 10 RemoveSynth
> in audition . (out 0) . (* 0.05) =<< lfdClipNoise AR f

> let f = xLine KR 1000 20000 10 RemoveSynth
> in audition . (out 0) . (* 0.05) =<< lfClipNoise AR f
