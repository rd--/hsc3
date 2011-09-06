> Sound.SC3.UGen.Help.viewSC3Help "LFNoise0"
> Sound.SC3.UGen.DB.ugenSummary "LFNoise0"

> import Sound.SC3.ID
> import qualified Sound.SC3.Monadic as M

> audition . (out 0) . (* 0.05) =<< M.lfNoise0 AR 1000

Modulate frequency.
> let {f = xLine KR 1000 10000 10 RemoveSynth
>     ;n = lfNoise0 'a' AR f}
> in audition (out 0 (n * 0.05))

Use as frequency control.
> let f = lfNoise0 'a' KR 4
> in audition (out 0 (sinOsc AR (f * 400 + 450) 0 * 0.1))
