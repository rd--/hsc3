> Sound.SC3.UGen.Help.viewSC3Help "SOS"
> Sound.SC3.UGen.DB.ugenSummary "SOS"

> import Sound.SC3

Same as TwoPole
> let { theta = line KR (0.2 * pi) pi 5 RemoveSynth
>     ; rho = line KR 0.6 0.99 5 RemoveSynth
>     ; b1 = 2 * rho * cos theta
>     ; b2 = - (rho * rho) }
> in audition (out 0 (sos (lfSaw AR 200 0 * 0.1) 1 0 0 b1 b2))
