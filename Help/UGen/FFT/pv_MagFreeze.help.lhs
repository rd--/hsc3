pv_MagClip buffer threshold

Clip bins to a threshold.  Clips bin magnitudes to a maximum
threshold.

> let { fileName = "/home/rohan/audio/metal.wav"
>     ; async h m = send h m >> wait h "/done" }
> in withSC3 (\fd -> do { async fd (b_alloc 10 2048 1)
>                       ; async fd (b_allocRead 12 fileName 0 0) })

> let { a = playBuf 1 12 (bufRateScale KR 12) 0 0 Loop
>     ; f = fft' 10 a
>     ; x = mouseX KR 0 1 Linear 0.1
>     ; h = pv_MagFreeze f (x >* 0.5) }
> in audition (out 0 (ifft' h * 0.5))

Synthesised input.

> let { a = sinOsc KR (squared (sinOsc KR 0.08 0 * 6 + 6.2)) 0 * 100 + 800
>     ; b = sinOsc AR a 0
>     ; f = fft' 10 b
>     ; x = mouseX KR 0 1 Linear 0.1
>     ; h = pv_MagFreeze f (x >* 0.5) }
> in audition (out 0 (ifft' h * 0.5))
