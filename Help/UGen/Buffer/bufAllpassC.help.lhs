> Sound.SC3.UGen.Help.viewSC3Help "BufAllpassC"
> Sound.SC3.UGen.DB.ugenSummary "BufAllpassC"

> import Sound.SC3.ID

Allocate buffer
> withSC3 (\fd -> async fd (b_alloc 0 44100 1))

Filtered decaying noise bursts
> let {d = dust 'a' AR 1
>     ;n = whiteNoise 'a' AR
>     ;x = decay d 0.2 * n * 0.25}
> in audition (out 0 (bufAllpassC 0 x 0.25 6))
