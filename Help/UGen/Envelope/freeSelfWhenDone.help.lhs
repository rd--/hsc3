> Sound.SC3.UGen.Help.viewSC3Help "FreeSelfWhenDone"
> Sound.SC3.UGen.DB.ugenSummary "FreeSelfWhenDone"

> import Sound.SC3

using RemoveSynth doneAction
> let {x = mouseX KR (-1) 1 Linear 0.1
>     ;e = linen x 1 0.1 1 RemoveSynth}
> in audition (out 0 (sinOsc AR 440 0 * e))

using FreeSelfWhenDone UGen
> let {x = mouseX KR (-1) 1 Linear 0.1
>     ;e = linen x 1 0.1 1 DoNothing}
> in audition (mrg [freeSelfWhenDone e, out 0 (sinOsc AR 440 0 * e)])
