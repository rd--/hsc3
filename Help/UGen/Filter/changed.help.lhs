> Sound.SC3.UGen.Help.viewSC3Help "Changed"
> Sound.SC3.UGen.DB.ugenSummary "Changed"

> import Sound.SC3.ID

simple composition of hpz1 and >*

> let {s = lfNoise0 'α' KR 2
>     ;c = changed s 0
>     ;c' = decay2 c 0.01 0.5
>     ;o = sinOsc AR (440 + mce2 s c' * 440) 0 * 0.1}
> in audition (out 0 o)
