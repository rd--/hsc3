> import Sound.SC3 {- hsc3 -}

> g_01 :: UId m => m UGen
> g_01 = do
>   r <- dustM KR 1
>   s <- dgeomM dinf (midiCPS 72) (midiRatio 1)
>   let t = impulse KR 10 0
>       f = demand t r s
>       o = sinOsc AR (mce [f,f + 0.7]) 0
>   return (max (cubed o) 0 * 0.1)

> g_02 =
>     let n = diwhite 'α' dinf 60 72
>         t = impulse KR 10 0
>         s = midiCPS n
>         f = demand t 0 s
>         o = sinOsc AR (mce [f,f + 0.7]) 0
>     in cubed (cubed o) * 0.1

audio rate (poll output is equal for x1 and x2)

> g_03 =
>     let i = lfNoise2 'α' AR 8000
>         d = dseq 'β' dinf (mce [i])
>         x = mouseX KR 1 3000 Exponential 0.2
>         t = impulse AR x 0
>         x1 = demand t 0 d
>         x2 = latch i t
>         s = mce2 x1 x2
>         p = poll t s 0 (mce2 (label "x1") (label "x2"))
>         o = sinOsc AR (s * 300 + 400) 0 * 0.1
>     in mrg2 o p

> g_04 =
>   let t = impulse AR 5 0
>       d1 = dseq 'α' dinf (mce [1,0,1,1,0,1,0,0,1,0,1])
>       d2 = dseq 'β' dinf (mce [0,1,0,0,1,0,1,1,0,1,0])
>       x = demand t 0 (mce2 d1 d2) * t
>   in decay x 1 * brownNoise 'γ' AR * 0.1

> g_05 =
>   let t = impulse AR 5 0
>       d = drand 'α' dinf (mce [dseq 'β' 1 (mce [1,1,1,1]),dseq 'γ' 1 (mce [1,0,0,0])])
>       x = demand t 0 d * t
>   in decay x 1 * brownNoise 'δ' AR * 0.1
