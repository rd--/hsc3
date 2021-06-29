> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Bindings.HW.External.Wavelets {- hsc3 -}

> g_01 =
>     let i = whiteNoise 'α' ar * 0.2
>         b = localBuf 'β' 2048 1
>         c = dwt b i 0.5 0 1 0 0
>         x = mouseX kr (-1) 1 Linear 0.1
>         c' = wt_FilterScale c x
>     in pan2 (idwt c' 0 0 0) x 1
