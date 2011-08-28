> Sound.SC3.UGen.Help.viewSC3Help "Line"
> Sound.SC3.UGen.DB.ugenSummary "Line"

#SC3
SC3 reorders the mul and add inputs to precede the doneAction input.

> import Sound.SC3

> let f = line KR 200 17000 5 RemoveSynth
> in audition (out 0 (sinOsc AR f 0 * 0.1))
