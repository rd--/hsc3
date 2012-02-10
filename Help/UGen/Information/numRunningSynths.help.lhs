> Sound.SC3.UGen.Help.viewSC3Help "NumRunningSynths"
> Sound.SC3.UGen.DB.ugenSummary "NumRunningSynths"

> import Sound.SC3

each concurrent audition increases oscillator frequency
> audition (out 0 (sinOsc AR (numRunningSynths * 200 + 400) 0 * 0.1))
