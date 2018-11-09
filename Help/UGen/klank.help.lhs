    > Sound.SC3.UGen.Help.viewSC3Help "Klank"
    > Sound.SC3.UGen.DB.ugenSummary "Klank"

> import Sound.SC3 {- hsc3 -}

The klankSpec family of functions can help create the 'spec' entry.

> f_01 impulse_freq reson_freq decay_time =
>   let u n = replicate (length reson_freq) n
>       k = klankSpec_k reson_freq (u 1) (u decay_time)
>   in klank (impulse AR impulse_freq 0 * 0.1) 1 0 1 k

> g_01 = f_01 2 [800,1071,1153,1723] 1

There is a limited form of multiple channel expansion possible at
'specification' input, below three equal dimensional specifications
are transposed and force expansion in a sensible manner.

> g_02 =
>     let u = [1,1,1,1]
>         p = [200,171,153,172]
>         q = [930,971,953,1323]
>         r = [8900,16062,9013,7892]
>         k = mce [klankSpec_k p u u,klankSpec_k q u u,klankSpec_k r u u]
>         s = mceTranspose k
>         i = mce [2,2.07,2.13]
>         t = impulse AR i 0 * 0.1
>         l = mce [-1,0,1]
>     in mix (pan2 (klank t 1 0 1 s) l 1)

Modal data

    > import Sound.SC3.Data.Modal {- hsc3-data -}
    > let Just reson_freq =lookup "Spinel sphere (diameter=3.6675mm)" modal_frequencies
    > audition (out 0 (f_01 0.125 reson_freq 16))
