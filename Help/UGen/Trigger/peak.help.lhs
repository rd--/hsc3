> Sound.SC3.UGen.Help.viewSC3Help "Peak"
> Sound.SC3.UGen.DB.ugenSummary "Peak"

> import Sound.SC3.ID

> let {t = dust 'α' AR 20
>     ;r = impulse AR 0.4 0
>     ;f = peak t r * 500 + 200}
> in audition (out 0 (sinOsc AR f 0 * 0.2))
