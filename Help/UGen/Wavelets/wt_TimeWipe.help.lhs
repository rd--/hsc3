wt_TimeWipe buffer wipe

Zero wavelet coefficients by time position in window.  Passes
particular scales while suppressing (zeroing coefficients) from others


buffer - dwt buffer.

wipe - can range between 0.0 and 1.0. Acts to zero out that proportion
of bins at each scale, starting from the leftmost (earliest in time).

> import Sound.SC3

> let {i = whiteNoise 'a' AR * 0.2
>     ;b = mrg2 (localBuf 'α' 2048 1) (maxLocalBufs 1)
>     ;c = dwt b i 0.5 0 1 0 0
>     ;x = mouseX' KR 0 1 Linear 0.1
>     ;c' = wt_TimeWipe c x}
> in audition (out 0 (pan2 (idwt c' 0 0 0) (x * 2 - 1) 1))
