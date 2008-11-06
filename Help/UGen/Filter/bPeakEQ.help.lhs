bPeakEQ i f rq db

    i - input signal to be processed
    f - center frequency
   rq - the reciprocal of Q, ie.  bandwidth / cutoffFreq
   db - boost/cut the center frequency (in dBs)

Parametric equalizer.

> import Sound.SC3

> let { i = soundIn (mce2 0 1)
>     ; f = mouseX KR 2200 18000 Exponential 0.2 
>     ; db = mouseY KR 12 (-12) Linear 0.2 }
> in audition (out 0 (bPeakEQ i f 0.8 db))

> let { i = soundIn (mce2 0 1)
>     ; f = mouseX KR 2200 18000 Exponential 0.2 
>     ; rq = mouseY KR 10 0.4 Linear 0.2 }
> in audition (out 0 (bPeakEQ i f rq 6))
