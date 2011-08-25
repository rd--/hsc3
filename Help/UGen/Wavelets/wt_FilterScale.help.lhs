wt_FilterScale buffer wipe

Low or high pass filter by wavelet scale.  Passes particular scales
while suppressing (zeroing coefficients) from others.


buffer - dwt buffer.

wipe - can range between -1 and +1. if wipe == 0 then there is no
effect. if wipe > 0 then it acts like a high pass filter, clearing
scales from the bottom up.  if wipe < 0 then it acts like a low pass
filter, clearing scales from the top down.

> import Sound.SC3

> let {i = whiteNoise 'a' AR * 0.2
>     ;b = mrg2 (localBuf 'α' 2048 1) (maxLocalBufs 1)
>     ;c = dwt b i 0.5 0 1 0 0
>     ;x = mouseX' KR (-1) 1 Linear 0.1
>     ;c' = wt_FilterScale c x}
> in audition (out 0 (pan2 (idwt c' 0 0 0) x 1))
