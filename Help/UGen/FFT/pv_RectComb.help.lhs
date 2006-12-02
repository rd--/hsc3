pv_RectComb buffer numTeeth phase width

> n <- whiteNoise AR
> withSC3 (\fd -> do send fd (b_alloc 10 2048 1)
>                    wait fd "/done")
> let x = mouseX KR 0 0.5 Linear 0.1
>     y = mouseY KR 0 0.5 Linear 0.1
>     c = pv_RectComb (fft 10 (n * 0.3)) 8 x y
> audition $ pan2 (ifft c) 0 1

> n <- whiteNoise AR
> withSC3 (\fd -> do send fd (b_alloc 10 2048 1)
>                    wait fd "/done")
> let p = lfTri KR 0.097 0 *   0.4  + 0.5
>     w = lfTri KR 0.240 0 * (-0.5) + 0.5
>     c = pv_RectComb (fft 10 (n * 0.3)) 8 p w
> audition $ pan2 (ifft c) 0 1
