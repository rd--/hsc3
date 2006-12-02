pv_BrickWall buffer wipe

`wipe' = a unit signal, from -1 to 0 the UGen acts as a low-pass
filter, from 0 to 1 it acts as a high pass filter.

> withSC3 (\fd -> do send fd (b_alloc 10 2048 1)
>                    wait fd "/done")
> n <- whiteNoise AR
> let x = mouseX KR (-1) 1 Linear 0.1
> audition $ ifft (pv_BrickWall (fft 10 (n * 0.2)) x)
