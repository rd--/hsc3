> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let n = drand 'α' dinf (mce [1, 3, 2, 7, 8])
>         x = mouseX KR 1 400 Exponential 0.1
>         t = impulse KR x 0
>         f = demand t 0 n * 30 + 340
>     in sinOsc AR f 0 * 0.1

> g_02 =
>   let d = drand 'α' dinf
>           (mce [dseq 'β' 1 (mce [2, 0, 2, 0, 1, 0, 1, 1])
>		 ,dseq 'γ' 1 (mce [2, 0, 1, 0, 1, 0, 1, 0])
>		 ,dseq 'δ' 1 (mce [2, 0, 1, 1, 1, 1, 1, 0])
>		 ,dseq 'ε' 1 (mce [2, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.3])
>		 ,dseq 'ζ' 1 (mce [2, 0, 0.3, 0, 0.3, 0, 0.3, 0])
>		 ,dseq 'η' 1 (mce [2, 0, 0, 1, 0, 0, 0, 0])
>		 ,dseq 'θ' 1 (mce [2, 0, 0, 0, 0, 0, 0, 0])
>		 ,dseq 'ι' 1 (mce [0, 1, 0, 1, 0, 1, 0, 1])
>		 ,dseq 'κ' 1 (mce [1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0])])
>       t = impulse AR 10 0
>       x = demand t 0 d * t
>   in decay x 1 * pinkNoise 'λ' AR * 0.1
