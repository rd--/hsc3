> Sound.SC3.UGen.Help.viewSC3Help "ToggleFF"
> Sound.SC3.UGen.DB.ugenSummary "ToggleFF"

> import Sound.SC3.ID

> let {t = dust 'a' AR (xLine KR 1 1000 60 DoNothing)
>     ;t' = toggleFF t * 400 + 800}
> in audition (out 0 (sinOsc AR t' 0 * 0.1))
