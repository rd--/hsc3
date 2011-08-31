> Sound.SC3.UGen.Help.viewSC3Help "Lag"
> Sound.SC3.UGen.DB.ugenSummary "Lag"

> import Sound.SC3

used to lag pitch
> let x = mouseX' KR 220 440 Linear 0.2
> in audition (out 0 (sinOsc AR (mce [x, lag x 1]) 0 * 0.1))
