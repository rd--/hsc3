bHiPass i f rq

   i - input signal to be processed
   f - cutoff frequency
  rq - the reciprocal of Q, ie. bandwidth / cutoffFreq

12db/oct rolloff - 2nd order resonant high pass filter.

> import Sound.SC3

> let { i = soundIn (mce2 0 1)
>     ; f = mouseX' KR 10 20000 Exponential 0.2
>     ; rq = mouseY' KR 0 1 Linear 0.2 }
> in audition (out 0 (bHiPass i f rq))
