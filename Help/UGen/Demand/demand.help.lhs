> Sound.SC3.UGen.Help.viewSC3Help "Demand"
> Sound.SC3.UGen.DB.ugenSummary "Demand"

> import Sound.SC3.ID
> import qualified Sound.SC3.Monadic as M

> do {r <- M.dust KR 1
>    ;s <- M.dgeom dinf (midiCPS 72) (midiRatio 1)
>    ;let {t = impulse KR 10 0
>         ;f = demand t r s
>         ;o = sinOsc AR (mce [f,f + 0.7]) 0}
>     in audition (out 0 (max (cubed o) 0 * 0.1))}

> let {n = diwhite 'a' dinf 60 72
>     ;t = impulse KR 10 0
>     ;s = midiCPS n
>     ;f = demand t 0 s
>     ;o = sinOsc AR (mce [f,f + 0.7]) 0}
> in audition (out 0 (cubed (cubed o) * 0.1))

audio rate (poll output is equal for x1 and x2)
> let {i = lfNoise2 'a' AR 8000
>     ;d = dseq 'a' dinf (mce [i])
>     ;x = mouseX KR 1 3000 Exponential 0.2
>     ;t = impulse AR x 0
>     ;x1 = demand t 0 d
>     ;x2 = latch i t
>     ;s = mce2 x1 x2
>     ;p = poll t s (mce2 (label "x1") (label "x2")) 0
>     ;o = sinOsc AR (s * 300 + 400) 0 * 0.1}
> in audition (mrg2 (out 0 o) p)
