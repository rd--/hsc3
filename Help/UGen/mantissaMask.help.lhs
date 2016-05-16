> Sound.SC3.UGen.Help.viewSC3Help "MantissaMask"
> Sound.SC3.UGen.DB.ugenSummary "MantissaMask"

> import Sound.SC3

> let s = sinOsc AR (sinOsc KR 0.2 0 * 400 + 500) 0 * 0.4
> in audition (out 0 (mantissaMask s 3))
