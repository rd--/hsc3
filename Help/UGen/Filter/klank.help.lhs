> Sound.SC3.UGen.Help.viewSC3Help "Klank"
> Sound.SC3.UGen.DB.ugenSummary "Klank"
# inputReordering: [4,0,1,2,3]

> import Sound.SC3 {- hsc3 -}

The function klankSpec can help create the 'spec' entry.

> let s = klankSpec' [800,1071,1153,1723] [1,1,1,1] [1,1,1,1]
> in audition (out 0 (klank (impulse AR 2 0 * 0.1) 1 0 1 s))

A variant spec function takes non-UGen inputs

> let {f = [800::Double,1071,1153,1723]
>     ;u = [1,1,1,1]
>     ;s = klankSpec' f u u}
> in audition (out 0 (klank (impulse AR 2 0 * 0.1) 1 0 1 s))

There is a limited form of multiple channel expansion possible at
'specification' input, below three equal dimensional specifications
are transposed and force expansion in a sensible manner.

> let {u = [1,1,1,1]
>     ;p = [200,171,153,172]
>     ;q = [930,971,953,1323]
>     ;r = [8900,16062,9013,7892]
>     ;k = mce [klankSpec' p u u,klankSpec' q u u,klankSpec' r u u]
>     ;s = mceTranspose k
>     ;i = mce [2,2.07,2.13]
>     ;t = impulse AR i 0 * 0.1
>     ;l = mce [-1,0,1]}
> in audition (out 0 (mix (pan2 (klank t 1 0 1 s) l 1)))
