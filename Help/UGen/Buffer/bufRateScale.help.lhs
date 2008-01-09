bufRateScale rate bufnum

Buffer rate scaling in respect to server samplerate.  Returns a
ratio by which the playback of a soundfile is to be scaled.

> withSC3 (\fd -> do { send fd (b_allocRead 0 "/home/rohan/audio/metal.wav" 0 0)
>                    ; wait fd "/done" })

> let { r = 1.25 * bufRateScale KR 0
>     ; p = phasor AR 0 r 0 (bufFrames KR 0) 0 }
> in audition (out 0 (bufRdL 1 AR 0 p NoLoop))
