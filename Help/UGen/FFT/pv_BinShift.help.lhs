pv_BinShift buffer stretch shift

Shift and scale the positions of the bins.  Can be used as a very
crude frequency shifter/scaler.  Shifts the leftmost bin at `buffer'
by `shift' places, the distance between subsequent bins is `stretch'.

> withSC3 (\fd -> do send fd (b_alloc 10 2048 1)
>                    wait fd "/done")
> let x  = mouseX KR (-10) 100 Linear 0.1
>     y  = mouseY KR 1 4 Linear 0.1
>     s0 = sinOsc KR 0.08 0 * 6 + 6.2
>     s1 = sinOsc KR (squared s0) 0 * 100 + 800
>     s2 = sinOsc AR s1 0
>     pv = pv_BinShift (fft 10 s2) y x
> audition $ pan2 (ifft pv) 0 0.5
