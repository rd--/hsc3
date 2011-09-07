> Sound.SC3.UGen.Help.viewSC3Help "PV_Invert"
> Sound.SC3.UGen.DB.ugenSummary "PV_Invert"

> import Sound.SC3.ID

> let { s = sinOsc AR 440 0 * 0.4
>     ; n = pinkNoise 'a' AR * 0.1
>     ; i = s + n
>     ; c0 = fft' 10 i
>     ; c1 = pv_Invert c0
>     ; run fd = do { _ <- async fd (b_alloc 10 2048 1)
>                   ; audition (out 0 (mce2 i (ifft' c1) * 0.5)) } }
> in withSC3 run
