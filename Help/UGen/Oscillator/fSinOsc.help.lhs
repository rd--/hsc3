> Sound.SC3.UGen.Help.viewSC3Help "FSinOsc"
> Sound.SC3.UGen.DB.ugenSummary "FSinOsc"

# SC2
The initial phase argument was not in the SC2 variant.

> import Sound.SC3

> audition (out 0 (fSinOsc AR (mce2 440 550) 0 * 0.05))

Modulate frequency
> audition (out 0 (fSinOsc AR (xLine KR 200 4000 1 RemoveSynth) 0 * 0.1))

Loses amplitude towards the end
> let f = fSinOsc AR (xLine KR 4 401 8 RemoveSynth)
> in audition (out 0 (fSinOsc AR (f 0 * 200 + 800) 0 * 0.1))
