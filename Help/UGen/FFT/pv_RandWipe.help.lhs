pv_RandWipe bufferA bufferB wipe trig

Cross fades between two sounds by copying bins in a random order.

bufferA = fft buffer A.  bufferB = fft buffer B.  wipe = copies
bins from bufferB in a random order (0, 1).  trig = select new
random ordering.

> withSC3 (\fd -> do send fd (b_alloc 10 2048 1)
>                    wait fd "/done"
>                    send fd (b_alloc 11 2048 1)
>                    wait fd "/done")
> let n0 = randomRs (400.0, 1000.0) (mkStdGen 0)
>     n1 = randomRs (80.0, 400.0) (mkStdGen 1)
>     n2 = randomRs (0.0, 8.0) (mkStdGen 2)
>     o0 = map (\n -> lfSaw AR n 0 * 0.1) (take 6 n0)
>     o1 = map (\n -> lfPulse AR n 0.0 0.2) (take 6 n1)
>     o2 = map (\n -> sinOsc KR n 0 * 0.2) (take 6 n2)
>     a  = mix (MCE o0)
>     b  = mix (MCE (zipWith (\p s -> p * (max s 0.0)) o1 o2))
>     f  = fft 10 a
>     g  = fft 11 b
>     x  = mouseX KR 0 1 Linear 0.1
>     y  = mouseY KR 0 1 Linear 0.1
> h <- pv_RandWipe f g x (y >* 0.5)
> audition (out 0 (pan2 (ifft h) 0 0.5))
