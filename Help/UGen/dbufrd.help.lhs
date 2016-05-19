    > Sound.SC3.UGen.Help.viewSC3Help "Dbufrd"
    > Sound.SC3.UGen.DB.ugenSummary "Dbufrd"

> import Sound.SC3 {- hsc3 -}
> import System.Random {- random -}

setup pattern at buffer 10

    > let n = randomRs (200.0,500.0) (mkStdGen 0)
    > in withSC3 (async (b_alloc_setn1 10 0 (take 24 n)))

pattern as frequency input

> g_01 =
>     let s = dseq 'α' 3 (mce [0,3,5,0,3,7,0,5,9])
>         b = dbrown 'β' 5 0 23 1
>         p = dseq 'γ' dinf (mce [s,b])
>         t = dust 'δ' KR 10
>         r = dbufrd 'ε' 10 p Loop
>     in sinOsc AR (demand t 0 r) 0 * 0.1

setup time pattern

    > let {i = randomRs (0,2) (mkStdGen 0)
    >     ;n = map ([1,0.5,0.25] !!) i}
    > in withSC3 (async (b_alloc_setn1 11 0 (take 24 n)))

requires buffers 10 and 11 as allocated above

> g_02 =
>     let s = dseq 'α' 3 (mce [0,3,5,0,3,7,0,5,9])
>         b = dbrown 'β' 5 0 23 1
>         p = dseq 'γ' dinf (mce [s,b])
>         j = dseries 'δ' dinf 0 1
>         d = dbufrd 'ε' 11 j Loop
>         l = dbufrd 'ζ' 10 p Loop
>         f = duty KR (d * 0.5) 0 DoNothing l
>     in sinOsc AR f 0 * 0.1

free buffers

    > withSC3 (async (b_free 10) >> async (b_free 11))
