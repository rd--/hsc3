> Sound.SC3.UGen.Help.viewSC3Help "GrainIn"
> Sound.SC3.UGen.DB.ugenSummary "GrainIn"

> import Sound.SC3.ID

> let {n = pinkNoise 'α' AR
>     ;x = mouseX KR (-0.5) 0.5 Linear 0.1
>     ;y = mouseY KR 5 25 Linear 0.1
>     ;t = impulse KR y 0
>     ;g = grainIn 2 t 0.1 n x (-1) 512 * 0.1}
> in audition (out 0 g)
