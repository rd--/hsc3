> Sound.SC3.UGen.Help.viewSC3Help "TGrains"
> Sound.SC3.UGen.DB.ugenSummary "TGrains"

> import Sound.SC3.ID

Load audio data
> let fn = "/home/rohan/data/audio/pf-c5.aif"
> in withSC3 (\fd -> async fd (b_allocRead 10 fn 0 0))

Mouse control
> let {tRate = mouseY' KR 2 200 Exponential 0.1
>     ;ctr = mouseX' KR 0 (bufDur KR 10) Linear 0.1
>     ;tr = impulse AR tRate 0}
> in audition (out 0 (tGrains 2 tr 10 1 ctr (4 / tRate) 0 0.1 2))

> let {b = 10
>     ;rt = mouseY' KR 8 120 Exponential 0.1
>     ;dur = 4 / rt
>     ;clk = dust 'a' AR rt
>     ;r = tRand 'a' 0 0.01 clk
>     ;pan = whiteNoise 'a' KR * 0.6
>     ;x = mouseX' KR 0 (bufDur KR b) Linear 0.1
>     ;pos = x + r}
> in audition (out 0 (tGrains 2 clk b 1 pos dur pan 0.1 2))

> let {b = 10
>     ;rt = mouseY' KR 2 120 Exponential 0.1
>     ;dur = 1.2 / rt
>     ;clk = impulse AR rt 0
>     ;pos = mouseX' KR 0 (bufDur KR b) Linear 0.1
>     ;n0 = whiteNoise 'a' KR
>     ;n1 = whiteNoise 'b' KR
>     ;rate = shiftLeft 1.2 (roundTo (n0 * 3) 1)}
> in audition (out 0 (tGrains 2 clk b rate pos dur (n1 * 0.6) 0.25 2))

Demand UGens as inputs (will eventually hang scsynth...)
> let {b = 10
>     ;rt = mouseY' KR 2 100 Linear 0.2
>     ;d e = dwhite e 1 0.1 0.2
>     ;z e0 e1 = drand e0 1 (mce [dgeom e0 (diwhite e0 1 20 40) 0.1 (1 + d e0)
>                                ,dgeom e1 (diwhite e1 1 20 40) 1 (1 - d e1)])
>     ;clk = impulse AR rt 0
>     ;dsq e xs = dseq e dinf (mce xs)
>     ;rate = dsq 'a' [1,1,z 'a' 'b',0.5,0.5,0.2,0.1,0.1,0.1,0.1] * 2 + 1
>     ;pos = dsq 'b' (take 8 (zipWith z ['a'..] ['A'..]))
>     ;dur = dsq 'c' [1,d 'x',1,z 'x' 'X',0.5,0.5,0.1,z 'y' 'Y'] * 2 / rt
>     ;pan = dsq 'd' [1,1,1,0.5,0.2,0.1,0,0,0] * 2 - 1
>     ;amp = dsq 'e' [1,0,z 'z' 'Z',0,2,1,1,0.1,0.1]}
> in audition (out 0 (tGrains 2 clk b rate pos dur pan amp 2))
