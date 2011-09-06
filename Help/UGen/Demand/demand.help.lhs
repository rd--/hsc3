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
