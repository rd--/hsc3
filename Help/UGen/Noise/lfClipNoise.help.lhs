> Sound.SC3.UGen.Help.viewSC3Help "LFClipNoise"
> Sound.SC3.UGen.DB.ugenSummary "LFClipNoise"

> import Sound.SC3

> audition . (out 0) . (* 0.05) =<< lfClipNoiseM AR 1000

Modulate frequency

> let f = xLine KR 1000 10000 10 RemoveSynth
> in audition . (out 0) . (* 0.05) =<< lfClipNoiseM AR f

Use as frequency control

> let n = lfClipNoise 'α' KR 4
> in audition (out 0 (sinOsc AR (n * 200 + 600) 0 * 0.1))
