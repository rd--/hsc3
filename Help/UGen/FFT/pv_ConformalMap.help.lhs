pv_ConformalMap buffer real imag

Applies the conformal mapping z -> (z-a)/(1-za*) to the phase vocoder
bins z with a given by the real and imag imputs to the UGen.

See http://mathworld.wolfram.com/ConformalMapping.html

buffer - buffer number of buffer to act on, passed in through a chain
real   - real part of a.
imag   - imaginary part of a.

> withSC3 (\fd -> do { send fd (b_alloc 10 1024 1)
>                    ; wait fd "/done" })

> let { i = in' 1 AR numOutputBuses * 0.5
>     ; x = mouseX KR (-1) 1 Linear 0.1
>     ; y = mouseY KR (-1) 1 Linear 0.1 }
> in audition (out 0 (pan2 (ifft' (pv_ConformalMap (fft' 10 i) x y)) 0 1))

With filtering.

> withSC3 (\fd -> do { send fd (b_alloc 0 2048 1)
>                    ; wait fd "/done" })

> let { o = mce [1, 1.1, 1.5, 1.78, 2.45, 6.7, 8] * 220
>     ; f = sinOsc KR (mce [0.16, 0.33, 0.41]) 0 * 10 + o
>     ; s = mix (lfSaw AR f 0) * 0.3
>     ; x = mouseX KR 0.01  2.0 Linear 0.1
>     ; y = mouseY KR 0.01 10.0 Linear 0.1
>     ; c = fft' 0 s
>     ; m = ifft' (pv_ConformalMap c x y) }
> in audition (out 0 (pan2 (combN m 0.1 0.1 10 * 0.5 + m) 0 1))
