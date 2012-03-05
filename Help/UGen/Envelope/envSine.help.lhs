> Sound.SC3.UGen.Help.viewSC3Help "Env.*sine"
> :t Sound.SC3.envSine

> import Sound.SC3

> let {s = envSine 9 0.1
>     ;e = envGen KR 1 1 0 1 RemoveSynth s}
> in audition (out 0 (sinOsc AR 440 0 * e))

> import qualified Graphics.Gnuplot.Simple as P {- gnuplot -}
> P.plotList [] (envelope_table 256 (envSine 9 1))
