> Sound.SC3.UGen.Help.viewSC3Help "XLine"
> Sound.SC3.UGen.DB.ugenSummary "XLine"

# SC3
At SC3 mul and add inputs precede the doneAction input.

> import Sound.SC3

> let {f = xLine KR 200 17000 10 RemoveSynth
>     ;o = sinOsc AR f 0 * 0.1}
> in audition (out 0 o)
