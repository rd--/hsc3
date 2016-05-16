> Sound.SC3.UGen.Help.viewSC3Help "TChoose"
> :t tChoose

# tChoose is a composite of tIRand and select.

> import Sound.SC3

> let {x = mouseX KR 1 1000 Exponential 0.1
>     ;t = dust 'a' AR x
>     ;f = midiCPS (tIRand 'b' 48 60 t)
>     ;o = let a = mce [sinOsc AR f 0
>                      ,saw AR (f * 2)
>                      ,pulse AR (f * 0.5) 0.1]
>           in tChoose 'c' t a}
> in audition (out 0 (o * 0.1))
