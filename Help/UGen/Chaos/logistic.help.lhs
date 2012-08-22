> Sound.SC3.UGen.Help.viewSC3Help "Logistic"
> Sound.SC3.UGen.DB.ugenSummary "Logistic"

> import Sound.SC3

SC3 default parameters
> audition (out 0 (logistic AR 3 1000 0.5))

Onset of chaos
> audition (out 0 (logistic AR (line KR 3.55 3.6 5 DoNothing) 1000 0.01))

Mouse control
> let {x = mouseX KR 3 3.99 Linear 0.1
>     ;y = mouseY KR 10 10000 Exponential 0.1}
> in audition (out 0 (logistic AR x y 0.25 * 0.5))
