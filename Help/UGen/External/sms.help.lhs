> Sound.SC3.UGen.Help.viewSC3Help "SMS"
> Sound.SC3.UGen.DB.ugenSummary "SMS"

> import Sound.SC3

sine reconstruction left channel, noises on right
> let {z= soundIn 4
>     ;y = mouseY KR 1 50 Linear 0.2
>     ;x = mouseX KR 0.5 4 Linear 0.2
>     ;o = sms z 50 y 8 0.3 x 0 0 0 1 (-1)}
> in audition (out 0 o)
