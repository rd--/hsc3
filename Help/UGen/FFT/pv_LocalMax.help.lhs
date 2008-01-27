pv_LocalMax buffer threshold

Pass bins which are a local maximum.  Passes only bins whose magnitude
is above a threshold and above their nearest neighbors.

buffer    - fft buffer.
threshold - magnitude threshold.

> let fileName = "/home/rohan/audio/metal.wav"
> in withSC3 (\fd -> do { async fd (b_alloc 10 2048 1)
>                       ; async fd (b_allocRead 12 fileName 0 0) })

> let { a = playBuf 1 12 (bufRateScale KR 12) 0 0 Loop
>     ; f = fft' 10 a
>     ; x = mouseX KR 0 100 Linear 0.1
>     ; h = pv_LocalMax f x }
> in audition (out 0 (ifft' h * 0.5))
