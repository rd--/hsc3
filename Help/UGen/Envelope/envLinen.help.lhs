> Sound.SC3.UGen.Help.viewSC3Help "Env.*linen"
> :t envLinen

> import Sound.SC3

> let {t = envLinen 0.4 2 0.4 0.1
>     ;e = envGen KR 1 1 0 1 RemoveSynth t}
> in audition (out 0 (sinOsc AR 440 0 * e))

> import qualified Graphics.Gnuplot.Simple as P {- gnuplot -}
> P.plotList [] (envelope_table 256 (envLinen 0.4 2 0.4 1))
