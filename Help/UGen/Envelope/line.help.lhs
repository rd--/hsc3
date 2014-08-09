> Sound.SC3.UGen.Help.viewSC3Help "Line"
> Sound.SC3.UGen.DB.ugenSummary "Line"

#SC3 reorders the mul and add inputs to precede the doneAction input.

> import Sound.SC3

> let f = line KR 200 17000 5 RemoveSynth
> in audition (out 0 (sinOsc AR f 0 * 0.1))

Demonstrate RemoveGroup done-action.

> withSC3 (send (g_new [(10,AddToTail,1)]))

> let f = line KR 200 (mce2 209 211) 5 RemoveGroup
> in audition_at (-1,AddToTail,10) (out 0 (sinOsc AR f 0 * 0.1))
