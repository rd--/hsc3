> Sound.SC3.UGen.Help.viewSC3Help "XOut"
> Sound.SC3.UGen.DB.ugenSummary "XOut"

> import Sound.SC3

Send signal to a bus, crossfading with existing contents.

> let {p a b = sinOsc AR (mce [a, b]) 0 * 0.1
>     ;x = mouseX KR 0 1 Linear 0.1
>     ;y = mouseY KR 0 1 Linear 0.1}
> in audition (mrg [out  0   (p 220 221)
>                  ,xOut 0 x (p 330 331)
>                  ,xOut 0 y (p 440 441)
>                  ,out  0   (p 120 121)])
