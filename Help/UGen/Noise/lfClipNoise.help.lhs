> Sound.SC3.UGen.Help.viewSC3Help "LFClipNoise"
> Sound.SC3.UGen.DB.ugenSummary "LFClipNoise"

> import Sound.SC3.ID
> import qualified Sound.SC3.Monadic as M

> audition . (out 0) . (* 0.05) =<< M.lfClipNoise AR 1000

Modulate frequency
> let f = xLine KR 1000 10000 10 RemoveSynth
> in audition . (out 0) . (* 0.05) =<< M.lfClipNoise AR f

Use as frequency control
> let n = lfClipNoise 'a' KR 4
> in audition (out 0 (sinOsc AR (n * 200 + 600) 0 * 0.1))
