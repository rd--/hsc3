    Sound.SC3.UGen.Help.viewSC3Help "TGrains"
    Sound.SC3.UGen.DB.ugenSummary "TGrains"

> import Sound.SC3 {- hsc3 -}

Load audio (#10) data

    > let fn = "/home/rohan/data/audio/pf-c5.aif"
    > withSC3 (async (b_allocRead 10 fn 0 0))

Mouse control

> g_01 =
>     let tRate = mouseY KR 2 200 Exponential 0.1
>         ctr = mouseX KR 0 (bufDur KR 10) Linear 0.1
>         tr = impulse AR tRate 0
>     in tGrains 2 tr 10 1 ctr (4 / tRate) 0 0.25 2

> g_02 =
>     let b = 10
>         rt = mouseY KR 8 120 Exponential 0.1
>         dur = 4 / rt
>         clk = dust 'α' AR rt
>         r = tRand 'β' 0 0.01 clk
>         pan = whiteNoise 'γ' KR * 0.6
>         x = mouseX KR 0 (bufDur KR b) Linear 0.1
>         pos = x + r
>     in tGrains 2 clk b 1 pos dur pan 0.25 2

> g_03 =
>     let b = 10
>         rt = mouseY KR 2 120 Exponential 0.1
>         dur = 1.2 / rt
>         clk = impulse AR rt 0
>         pos = mouseX KR 0 (bufDur KR b) Linear 0.1
>         n0 = whiteNoise 'α' KR
>         n1 = whiteNoise 'β' KR
>         rate = shiftLeft 1.2 (roundTo (n0 * 3) 1)
>     in tGrains 2 clk b rate pos dur (n1 * 0.6) 0.25 2

Demand UGens as inputs (may eventually hang scsynth?)

> g_04 =
>     let b = 10
>         rt = mouseY KR 2 100 Linear 0.2
>         d e = dwhite e 1 0.1 0.2
>         z e0 e1 = drand e0 1 (mce [dgeom e0 (diwhite e0 1 20 40) 0.1 (1 + d e0)
>                                   ,dgeom e1 (diwhite e1 1 20 40) 1 (1 - d e1)])
>         clk = impulse AR rt 0
>         dsq e xs = dseq e dinf (mce xs)
>         rate = dsq 'α' [1,1,z 'β' 'γ',0.5,0.5,0.2,0.1,0.1,0.1,0.1] * 2 + 1
>         pos = dsq 'δ' (take 8 (zipWith z ['ε'..] ['ζ'..]))
>         dur = dsq 'η' [1,d 'θ',1,z 'ι' 'κ',0.5,0.5,0.1,z 'λ' 'μ'] * 2 / rt
>         pan = dsq 'ν' [1,1,1,0.5,0.2,0.1,0,0,0] * 2 - 1
>         amp = dsq 'ξ' [1,0,z 'ο' 'π',0,2,1,1,0.1,0.1]
>     in tGrains 2 clk b rate pos dur pan amp 2
