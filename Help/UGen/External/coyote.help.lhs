> Sound.SC3.UGen.Help.viewSC3Help "Coyote"
> Sound.SC3.UGen.DB.ugenSummary "Coyote"

> import Sound.SC3.ID

> let {i = soundIn 4
>     ;c = coyote KR i 0.2 0.2 0.01 0.5 0.05 0.05
>     ;o = pinkNoise 'a' AR * decay c 1}
> in audition (out 0 (mce2 i o))
