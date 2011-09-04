> Sound.SC3.UGen.Help.viewSC3Help "VOSIM"
> Sound.SC3.UGen.DB.ugenSummary "VOSIM"

> import Sound.SC3.ID

> let {p = tRand 'a' 0 1 (impulse AR 6 0)
>     ;t = impulse AR (9 * ( 1 + ( p >* 0.95))) 0
>     ;x = mouseX' KR 0.25 2 Linear 0.2
>     ;y = mouseY' KR 0.25 1.5 Linear 0.2
>     ;z = 9
>     ;rng l r i = linLin i (-1) 1 l r
>     ;mk_n e = rng 0.25 2 (lfNoise2 e KR z)
>     ;tR e l r = tRand e (mce l) (mce r)
>     ;f = tR 'b' [40,120,220] [440,990,880] t
>     ;n = tR 'b' [4] [8,16,32] t
>     ;d = tR 'b' [0.2,0.4,0.6] [0.6,0.8,1] t
>     ;a = tR 'b' [0] [0.2,0.6,1] t
>     ;l = tR 'b' [-1] [1] t
>     ;xn = mk_n 'c'
>     ;yn = mk_n 'd'
>     ;v = vosim t (f * x * xn) n (d * y * yn) * a}
> in audition (out 0 (pan2 (mix v) l 1))
