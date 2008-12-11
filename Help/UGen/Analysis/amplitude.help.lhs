amplitude r i at rt

    r - operating rate
    i - input
   at - attack time (0.01)
   rt - release time (0.01)

Amplitude follower. Tracks the peak amplitude of a signal.

> let { s = in' 1 AR numOutputBuses
>     ; a = amplitude KR s 0.1 0.1 }
> in audition (out 0 (pulse AR 90 0.3 * a))

> let { s = in' 1 AR numOutputBuses
>     ; f = amplitude KR s 0.1 0.1 * 1200 + 400 }
> in audition (out 0 (sinOsc AR f 0 * 0.3))
