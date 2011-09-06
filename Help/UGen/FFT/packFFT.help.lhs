> Sound.SC3.UGen.Help.viewSC3Help "PackFFT"
> Sound.SC3.UGen.DB.ugenSummary "PackFFT"

> import Sound.SC3.ID

> withSC3 (\fd -> send fd (b_alloc 10 512 1))

> let {n = 100
>     ;square a = a * a
>     ;r1 = let f = expRand 'a' 0.1 1
>           in linLin (fSinOsc KR f 0) (-1) 1 0 1
>     ;m1 = udup' n r1
>     ;m2 = zipWith (*) m1 (map square [1.0, 0.99 ..])
>     ;r2 = let r = iRand 'a' (-3) 5
>           in lfPulse KR (2 ** r) 0 0.3
>     ;i = udup' n r2
>     ;m3 = zipWith (*) m2 i
>     ;p = replicate n 0.0
>     ;c1 = fft' 10 (fSinOsc AR 440 0)
>     ;c2 = packFFT c1 512 0 (constant n - 1) 1 (packFFTSpec m3 p)
>     ;s = ifft' c2}
> in audition (out 0 (mce [s,s]))
