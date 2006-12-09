pv_MagBelow buffer threshold

Pass bins below a threshold.  Pass only bands where the magnitude is
below `threshold'.  This value is not normalized and is therefore
dependant on the buffer size.

> let fileName = "/home/rohan/sw/sw-01/audio/metal.wav"
> withSC3 (\fd -> do send fd (b_alloc 10 2048 1)
>                    wait fd "/done"
>                    send fd (b_allocRead 12 fileName 0 0)
>                    wait fd "/done")
> let a = playBuf 1 AR 12 (bufRateScale KR 12) 0 0 Loop
>     f = fft 10 a
>     x = mouseX KR 0 100 Linear 0.1
>     h = pv_MagBelow f x
> audition $ ifft h * 0.5

Synthesised input.

> withSC3 (\fd -> do send fd (b_alloc 10 2048 1)
>                    wait fd "/done")
> let a = sinOsc KR (squared (sinOsc KR 0.08 0 * 6 + 6.2)) 0 * 100 + 800
>     b = sinOsc AR a 0
>     f = fft 10 b
>     x = mouseX KR 0 1024 Linear 0.1
>     h = pv_MagBelow f x
> audition $ ifft h * 0.5
