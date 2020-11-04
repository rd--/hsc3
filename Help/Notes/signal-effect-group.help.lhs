> import Sound.OSC {- hosc -}
> import Sound.SC3 {- hsc3 -}

Signal/effect model using separate groups operating at the same bus.

> n_01 =
>     let d = dust 'α' AR 1
>         n = whiteNoise 'β' AR
>         i = decay (d * 0.5) 0.2 * n
>     in synthdef "n_01" (out 0 i)

> n_02 =
>     let i = in' 1 AR 0
>     in synthdef "n_02" (replaceOut 0 (combC i 0.2 0.2 3))

> m_01 =
>     [g_new [(1,AddToTail,0)]
>     ,g_new [(2,AddToTail,0)]
>     ,d_recv n_01
>     ,d_recv n_02
>     ,s_new "n_01" (-1) AddToTail 1 []
>     ,s_new "n_02" (-1) AddToTail 2 []]

> f_01 m =
>     if isAsync m
>     then async m >> return ()
>     else sendMessage m

    withSC3 (mapM_ f_01 m_01)
