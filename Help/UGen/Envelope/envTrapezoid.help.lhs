> :t Sound.SC3.envTrapezoid

> import Sound.SC3

> let { t = envTrapezoid 0.05 0.95 3 0.1
>     ; e = envGen KR 1 1 0 1 RemoveSynth t }
> in audition (out 0 (sinOsc AR 440 0 * e))

> let e = [0,3,-99,-99,0.1,0.5,1,0,0.1,0,1,0,0,1.5,1,0]
> in envelope_sc3_array (envTrapezoid 0 0.25 2 0.1) == Just e

> import qualified Graphics.Gnuplot.Simple as P {- gnuplot -}
> P.plotList [] (envelope_table 256 (envTrapezoid 0.75 0.25 2 1))
