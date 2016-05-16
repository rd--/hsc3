> Sound.SC3.UGen.Help.viewSC3Help "Poll"
> Sound.SC3.UGen.DB.ugenSummary "Poll"

> import Sound.SC3 {- hsc3 -}

> let {t = impulse KR 10 0
>     ;l = line KR 0 1 1 RemoveSynth}
> in audition (poll t l (label "polling...") 0)

multichannel expansion (requires labels be equal length...)

> let {t = impulse KR (mce2 10 5) 0
>     ;l = line KR 0 (mce2 1 5) (mce2 1 2) DoNothing}
> in audition (poll t l (mce2 (label "t1") (label "t2")) 0)
