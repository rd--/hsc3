bufFrames rate bufnum

Current duration of buffer.

> withSC3 (\fd -> do send fd (b_allocRead 0 "/home/rohan/audio/metal.wav" 0 0)
>                    wait fd "/done")

> let p = phasor AR 0 (bufRateScale KR 0) 0 (bufFrames KR 0) 0
> audition (out 0 (bufRdL 1 AR 0 p NoLoop))

> let p = k2A (mouseX KR 0 (bufFrames KR 0) Linear 0.1)
> audition (out 0 (bufRdL 1 AR 0 p NoLoop))
