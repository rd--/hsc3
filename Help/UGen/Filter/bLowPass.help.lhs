bLowPass i f rq
bLowPassCoef sr f rq

   i - input signal to be processed
  sr - sample rate
   f - cutoff frequency
  rq - the reciprocal of Q, ie. bandwidth / cutoffFreq

12db/oct rolloff - 2nd order resonant low pass filter.

> import Sound.SC3

> let { i = soundIn (mce2 0 1)
>     ; f = mouseX' KR 10 20000 Exponential 0.2
>     ; rq = mouseY' KR 0 1 Linear 0.2 }
> in audition (out 0 (bLowPass i f rq))

> let { i = mix (saw AR (mce [0.99, 1, 1.01] * 440) * 0.3)
>     ; f = mouseX' KR 100 20000 Exponential 0.2
>     ; rq = mouseY' KR 0.1 1 Linear 0.2 }
> in audition (out 0 (bLowPass i f rq))

Calculate coefficients and use sos.

> let { i = mix (saw AR (mce [0.99, 1, 1.01] * 440) * 0.3)
>     ; f = mouseX' KR 100 20000 Exponential 0.2
>     ; rq = mouseY' KR 0.1 1 Linear 0.2
>     ; (a0, a1, a2, b1, b2) = bLowPassCoef sampleRate f rq
>     ; flt ip = sos ip a0 a1 a2 b1 b2 }
> in audition (out 0 (flt (flt i)))
