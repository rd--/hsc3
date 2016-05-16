> Sound.SC3.UGen.Help.viewSC3Help "AY"
> Sound.SC3.UGen.DB.ugenSummary "AY"

> import Sound.SC3

> audition (out 0 (ay 1777 1666 1555 1 7 15 15 15 4 1 0))

> let { tonea = mouseY KR 10 3900 Exponential 0.2
>     ; toneb = mouseX KR 10 3900 Exponential 0.2
>     ; ctl = 3
>     ; vola = 14
>     ; volb = 14
>     ; volc = 0
>     ; s = ay tonea toneb 1555 1 ctl vola volb volc 4 1 0 }
> in audition (out 0 (pan2 s 0 0.25))

> let {rate = mouseX KR 0.1 10 Linear 0.2
>     ;rng l r i = return (linLin i (-1) 1 l r)
>     ;mk_ctl l r = lfdNoise3M KR rate >>= rng l r
>     ;mk_ctl_0 l r = lfdNoise0M KR rate >>= rng l r}
> in do {tonea <- mk_ctl 10 3900
>       ;toneb <- mk_ctl 10 3900
>       ;tonec <- mk_ctl 10 3900
>       ;n <- mk_ctl 0 31
>       ;ctl <- mk_ctl_0 0 31
>       ;vola <- mk_ctl 0 15
>       ;volb <- mk_ctl 0 15
>       ;volc <- mk_ctl 0 15
>       ;efreq <- mk_ctl 0 4095
>       ;estyle <- mk_ctl 0 15
>       ;let s = ay tonea toneb tonec n ctl vola volb volc efreq estyle 0
>        in audition (out 0 (pan2 s 0 0.5))}
