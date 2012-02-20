> Sound.SC3.UGen.Help.viewSC3Help "DC"
> Sound.SC3.UGen.DB.ugenSummary "DC"

> import Sound.SC3

nothing
> audition (out 0 0)
> withSC3 (\fd -> send fd (n_trace [-1]))

zero
> audition (out 0 (dc AR 0))

DC offset; will click on start and finish
> audition (out 0 (dc AR 0.5))
> audition (out 0 (0.5 + sinOsc AR 440 0 * 0.1))
> audition (out 0 (dc AR 0.5 + sinOsc AR 440 0 * 0.1))

Transient before LeakDC adapts and suppresses the offset?
> audition (out 0 (dc AR 1))
> audition (out 0 (leakDC (dc AR 1) 0.995))
