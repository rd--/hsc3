pv_RectComb buffer numTeeth phase width

> withSC3 (\fd -> async fd (b_alloc 10 2048 1))

> do { n <- whiteNoise AR
>    ; let { x = mouseX KR 0 0.5 Linear 0.1
>          ; y = mouseY KR 0 0.5 Linear 0.1
>          ; c = pv_RectComb (fft' 10 (n * 0.3)) 8 x y }
>      in audition (out 0 (pan2 (ifft' c) 0 1)) }

> do { n <- whiteNoise AR
>    ; let { p = lfTri KR 0.097 0 *   0.4  + 0.5
>          ; w = lfTri KR 0.240 0 * (-0.5) + 0.5
>          ; c = pv_RectComb (fft' 10 (n * 0.3)) 8 p w }
>      in audition (out 0 (pan2 (ifft' c) 0 1)) }
