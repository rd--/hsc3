> Sound.SC3.UGen.Help.viewSC3Help "LFDClipNoise"
> Sound.SC3.UGen.DB.ugenSummary "LFDClipNoise"

> import Sound.SC3.ID
> import qualified Sound.SC3.Monadic as M

for fast x lfClipNoise frequently seems stuck, lfdClipNoise changes smoothly
> let {x = mouseX KR 0.1 1000 Exponential 0.2
>     ;n = lfdClipNoise 'a' AR x}
> in audition (out 0 (sinOsc AR (n * 200 + 500) 0 * 0.05))

> let {x = mouseX KR 0.1 1000 Exponential 0.2
>     ;n = lfClipNoise 'a' AR x}
> in audition (out 0 (sinOsc AR (n * 200 + 500) 0 * 0.05))

lfClipNoise quantizes time steps at high freqs, lfdClipNoise does not:
> let f = xLine KR 1000 20000 10 RemoveSynth
> in audition . (out 0) . (* 0.05) =<< M.lfdClipNoise AR f

> let f = xLine KR 1000 20000 10 RemoveSynth
> in audition . (out 0) . (* 0.05) =<< M.lfClipNoise AR f
