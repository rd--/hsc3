lfdNoise0 freq

Dynamic step noise. Like lfNoise0, it generates random values at a
rate given by the freq argument, with two differences: no time
quantization, and fast recovery from low freq values.

lfNoise0,1,2 quantize to the nearest integer division of the
samplerate, and they poll the freq argument only when scheduled, and
thus seem to hang when freqs get very low.

If you don't need very high or very low freqs, or use fixed freqs,
LFNoise0 is more efficient.

Try wiggling mouse quickly; LFNoise frequently seems stuck,
LFDNoise changes smoothly.

> let x = mouseX KR 0.1 1000 Exponential 0.2
> audition . (* 0.1) =<< lfdNoise0 AR x

> let x = mouseX KR 0.1 1000 Exponential 0.2
> audition . (* 0.1) =<< lfNoise0 AR x

silent for 2 secs before going up in freq

> let f = xLine KR 0.5 10000 3 RemoveSynth
> audition . (* 0.1) =<< lfdNoise0 AR f

> let f = xLine KR 0.5 10000 3 RemoveSynth
> audition . (* 0.1) =<< lfNoise0 AR f

LFNoise quantizes time steps at high freqs, LFDNoise does not:

> let f = xLine KR 1000 20000 10 RemoveSynth
> audition . (* 0.1) =<< lfdNoise0 AR f

> let f = xLine KR 1000 20000 10 RemoveSynth
> audition . (* 0.1) =<< lfNoise0 AR f

