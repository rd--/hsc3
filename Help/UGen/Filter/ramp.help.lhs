ramp in lagTime

Linear lag.  This is similar to Lag but with a linear rather than
exponential lag. This is useful for smoothing out control signals.

       in - input signal
  lagTime - 60 dB lag time in seconds

Used to lag pitch

> import Sound.SC3

> let { o = lfPulse KR 4 0 0.5 * 50 + 400
>     ; l = line KR 0 1 15 DoNothing
>     ; f = ramp o l }
> in audition (out 0 (sinOsc AR f 0 * 0.3))
