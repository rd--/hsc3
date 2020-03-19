> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let n = whiteNoise 'α' KR
>         x = mouseX KR (-0.5) 0.5 Linear 0.1
>         y = mouseY KR 0 400 Linear 0.1
>         f = n * y + 440
>         t = impulse KR 10 0
>     in grainSin 2 t 0.1 f x (-1) 512 * 0.1

-- <https://www.listarc.bham.ac.uk/lists/sc-users/msg66911.html>

> f_02 k =
>   let t = impulse AR (mouseY KR 1 999 Linear 0.2) 0
>	f i = ((fromIntegral i ** range_hs (0.3,0.7) (lfNoise0 i KR 1)) + 1) * 99
>       l = mce (map f [0::Int .. k - 1])
>   in grainSin 2 t (mouseX KR 0.001 0.5 Exponential 0.2) (tChoose 'α' t l) 0 (-1) 512 * 0.01

> g_02 = f_02 16

> g_03 =
>   let (s1,s2) = unmce2 g_02
>   in freeVerb2 s1 s2 0.9 0.1 0.5

