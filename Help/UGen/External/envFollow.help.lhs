> Sound.SC3.UGen.Help.viewSC3Help "EnvFollow"
> Sound.SC3.UGen.DB.ugenSummary "EnvFollow"

> import Sound.SC3.ID

> let {i = soundIn 4
>     ;c = envFollow KR i 0.99
>     ;o = pinkNoise 'a' AR * c}
> in audition (out 0 (mce2 i o))
