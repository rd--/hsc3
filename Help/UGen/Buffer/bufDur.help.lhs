bufDur rate bufnum

Current duration of buffer.

> withSC3 (\fd -> do send fd (b_allocRead 0 "/home/rohan/sw/sw-01/audio/metal.wav" 0 0)
>                    wait fd "/done")

> let t = impulse AR (recip (bufDur KR 0)) 0
>     p = sweep t (bufSampleRate KR 0)
> audition (out 0 (bufRdL 1 AR 0 p NoLoop))
