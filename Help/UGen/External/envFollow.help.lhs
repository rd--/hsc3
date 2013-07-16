> Sound.SC3.UGen.Help.viewSC3Help "EnvFollow"
> Sound.SC3.UGen.DB.ugenSummary "EnvFollow"

> import Sound.SC3.ID

> let {z = soundIn 4
>     ;d = mouseX KR 0.990 0.999 Linear 0.2
>     ;c = envFollow KR z d
>     ;o = pinkNoise 'α' AR * c}
> in audition (out 0 (mce2 z o))
