> Sound.SC3.UGen.Help.viewSC3Help "Formlet"
> Sound.SC3.UGen.DB.ugenSummary "Formlet"

> import Sound.SC3 {- hsc3 -}

> audition (out 0 (formlet (impulse AR 20 0.5) 1000 0.01 0.1))

> let f = xLine KR 10 400 8 RemoveSynth
> in audition (out 0 (formlet (blip AR f 1000 * 0.1) 1000 0.01 0.1))

Modulating formant frequency.

> let {s = blip AR (sinOsc KR 5 0 * 20 + 300) 1000 * 0.1
>     ;ff = xLine KR 1500 700 8 RemoveSynth}
> in audition (out 0 (formlet s ff 0.005 0.04))

Mouse control of frequency and decay time.

> let {s = blip AR (sinOsc KR 5 0 * 20 + 300) 1000 * 0.1
>     ;x = mouseX KR 0.01 0.2 Exponential 0.2
>     ;y = mouseY KR 700 2000 Exponential 0.2
>     ;o = formlet s y 0.005 x}
> in audition (out 0 o)

and again...

> let {s = dust 'α' KR (mce2 10 11)
>     ;x = mouseX KR 0.1 2 Exponential 0.2
>     ;y = mouseY KR 7 200 Exponential 0.2
>     ;f = formlet s y 0.005 x
>     ;o = sinOsc AR (f * 200 + mce2 500 600 - 100) 0 * 0.2}
> in audition (out 0 o)
