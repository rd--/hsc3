> Sound.SC3.UGen.Help.viewSC3Help "Env.*triangle"
> :t envTriangle

> import Sound.SC3

> let {t = envTriangle 1 0.1
>     ;e = envGen KR 1 1 0 1 RemoveSynth t}
> in audition (out 0 (sinOsc AR 440 0 * e))

> import qualified Graphics.Gnuplot.Simple as P {- gnuplot -}
> P.plotList [] (envelope_table 256 (envTriangle 1 1))
