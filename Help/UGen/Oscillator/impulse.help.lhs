> Sound.SC3.UGen.Help.viewSC3Help "Impulse"
> Sound.SC3.UGen.DB.ugenSummary "Impulse"

> import Sound.SC3

> audition (out 0 (impulse AR 800 0 * 0.1))

> let f = xLine KR 800 10 5 RemoveSynth
> in audition (out 0 (impulse AR f 0.0 * 0.1))

> let {f = mouseY KR 4 8 Linear 0.1
>     ;x = mouseX KR 0 1 Linear 0.1}
> in audition (out 0 (impulse AR f (mce [0,x]) * 0.1))

An impulse with frequency 0 returns a single impulse
> audition (out 0 (decay (impulse AR 0 0) 1 * brownNoise 'a' AR * 0.1))
