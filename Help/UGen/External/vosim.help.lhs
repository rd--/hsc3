> Sound.SC3.UGen.Help.viewSC3Help "VOSIM"
> Sound.SC3.UGen.DB.ugenSummary "VOSIM"

> import Sound.SC3

> let {p = tRand 'α' 0 1 (impulse AR 6 0)
>     ;t = impulse AR (9 * ( 1 + ( p >* 0.95))) 0
>     ;x = mouseX KR 0.25 2 Linear 0.2
>     ;y = mouseY KR 0.25 0.75 Linear 0.2
>     ;z = 9
>     ;rng i = linLin i (-1) 1
>     ;mk_n e = rng (lfNoise2 e KR z) 0.25 2
>     ;tR e ll rl = tRand e (mce ll) (mce rl)
>     ;f = tR 'β' [40,120,220] [440,990,880] t
>     ;n = tR 'γ' [4] [8,16,32] t
>     ;d = tR 'δ' [0.2,0.4,0.6] [0.6,0.8,1] t
>     ;a = tR 'ε' [0] [0.05,0.15,0.25] t
>     ;l = tR 'ζ' [-1] [1] t
>     ;xn = mk_n 'η'
>     ;yn = mk_n 'θ'
>     ;v = vosim t (f * x * xn) n (d * y * yn) * a}
> in audition (out 0 (pan2 (mix v) l 1))
