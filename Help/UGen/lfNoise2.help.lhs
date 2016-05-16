> Sound.SC3.UGen.Help.viewSC3Help "LFNoise2"
> Sound.SC3.UGen.DB.ugenSummary "LFNoise2"

> import Sound.SC3

> audition . (out 0) . (* 0.05) =<< lfNoise2M AR 1000

Modulate frequency.

> let {f = xLine KR 1000 10000 10 RemoveSynth
>     ;n = lfNoise2 'α' AR f}
> in audition (out 0 (n * 0.05))

Use as frequency control.

> let f = lfNoise2 'α' KR 4
> in audition (out 0 (sinOsc AR (f * 400 + 450) 0 * 0.1))

Drawings

> import Sound.SC3.Plot {- hsc3-plot -}

> plot_ugen1 0.1 (lfNoise2 'α' AR 1000)
