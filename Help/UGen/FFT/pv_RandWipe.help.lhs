> Sound.SC3.UGen.Help.viewSC3Help "PV_RandWipe"
> Sound.SC3.UGen.DB.ugenSummary "PV_RandWipe"

> import Sound.SC3.ID
> import qualified System.Random as R

> withSC3 (do {_ <- async (b_alloc 10 2048 1)
>             ;async (b_alloc 11 2048 1)})

> let { n0 = R.randomRs (400.0, 1000.0) (R.mkStdGen 0)
>     ; n1 = R.randomRs (80.0, 400.0) (R.mkStdGen 1)
>     ; n2 = R.randomRs (0.0, 8.0) (R.mkStdGen 2)
>     ; o0 = map (\n -> lfSaw AR n 0 * 0.1) (take 6 n0)
>     ; o1 = map (\n -> lfPulse AR n 0.0 0.2) (take 6 n1)
>     ; o2 = map (\n -> sinOsc KR n 0 * 0.2) (take 6 n2)
>     ; a = mix (mce o0)
>     ; b = mix (mce (zipWith (\p s -> p * (max s 0.0)) o1 o2))
>     ; f = fft' 10 a
>     ; g = fft' 11 b
>     ; x = mouseX KR 0 1 Linear 0.1
>     ; y = mouseY KR 0 1 Linear 0.1
>     ; h = pv_RandWipe 'a' f g x (y >* 0.5) }
> in audition (out 0 (pan2 (ifft' h) 0 0.5))
