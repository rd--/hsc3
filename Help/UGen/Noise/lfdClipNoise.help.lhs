> Sound.SC3.UGen.Help.viewSC3Help "LFDClipNoise"
> Sound.SC3.UGen.DB.ugenSummary "LFDClipNoise"

> import Sound.SC3

for fast x lfClipNoise frequently seems stuck, lfdClipNoise changes smoothly

> let {x = mouseX KR 0.1 1000 Exponential 0.2
>     ;n = lfdClipNoise 'α' AR x}
> in audition (out 0 (sinOsc AR (n * 200 + 500) 0 * 0.05))

> let {x = mouseX KR 0.1 1000 Exponential 0.2
>     ;n = lfClipNoise 'α' AR x}
> in audition (out 0 (sinOsc AR (n * 200 + 500) 0 * 0.05))

lfClipNoise quantizes time steps at high freqs, lfdClipNoise does not:

> let f = xLine KR 1000 20000 10 RemoveSynth
> in audition . (out 0) . (* 0.05) =<< lfdClipNoiseM AR f

> let f = xLine KR 1000 20000 10 RemoveSynth
> in audition . (out 0) . (* 0.05) =<< lfClipNoiseM AR f
