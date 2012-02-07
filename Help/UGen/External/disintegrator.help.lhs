> Sound.SC3.UGen.Help.viewSC3Help "Disintegrator"
> Sound.SC3.UGen.DB.ugenSummary "Disintegrator"

> import Sound.SC3

> let {x = mouseX KR 0 1 Linear 0.2
>     ;y = mouseY KR 0 1 Linear 0.2
>     ;s = sinOsc AR (mce2 400 404) 0 * 0.2
>     ;o = disintegrator 'a' s x y}
> in audition (out 0 o)
