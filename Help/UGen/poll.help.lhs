    > Sound.SC3.UGen.Help.viewSC3Help "Poll"
    > Sound.SC3.UGen.DB.ugenSummary "Poll"

> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let t = impulse KR 10 0
>         l = line KR 0 1 1 RemoveSynth
>     in poll t l 0 (label "polling...")

multichannel expansion (requires labels be equal length...)

> g_02 =
>     let t = impulse KR (mce2 10 5) 0
>         l = line KR 0 (mce2 1 5) (mce2 1 2) DoNothing
>     in poll t l 0 (mce2 (label "t1") (label "t2"))

poll will not poll once with a trigger of one, use impulse with frequency zero

> g_03 =
>   let k = control KR "k" 0.3
>       x = negate (k * 1.1)
>       t = impulse KR 0 0 {- 1 -}
>   in mrg2 x (poll t x (-1) (label "x"))

poll at trigger control

> g_04 =
>   let t = tr_control "t" 0.3
>       f1 = lfNoise2 'α' AR 0.25 * 100 + 110
>       f2 = lfNoise2 'β' AR 0.25 * 200 + 220
>       s = gendy1 'γ' AR 1 1 1 1 f1 f2 0.5 0.5 12 0 * 0.1
>       p = poll t (mce2 f1 f2) (-1) (mce2 (label "f1") (label "f2"))
>   in mrg2 s p

    import Sound.OSC {- hosc -}
    withSC3 (sendMessage (n_set1 (-1) "t" 1))
