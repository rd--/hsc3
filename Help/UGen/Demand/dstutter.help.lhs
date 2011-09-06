> Sound.SC3.UGen.Help.viewSC3Help "Dstutter"
> Sound.SC3.UGen.DB.ugenSummary "Dstutter"

> import Sound.SC3.ID

> let {inp = dseq 'a' dinf (mce [1,2,3])
>     ;nse = diwhite 'a' dinf 2 8
>     ;rep = dstutter 'a' nse inp
>     ;trg = impulse KR (mouseX' KR 1 40 Exponential 0.2) 0
>     ;frq = demand trg 0 rep * 30 + 340}
> in audition (out 0 (sinOsc AR frq 0 * 0.1))
