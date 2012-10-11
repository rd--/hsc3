> Sound.SC3.UGen.Help.viewSC3Help "VarLag"
> Sound.SC3.UGen.DB.ugenSummary "VarLag"

#sc3
SC3 is a composite UGen, hsc3 is a direct binding to the underlying UGen.

> import Sound.SC3

used to lag pitch
> let x = mouseX KR 220 440 Linear 0.2
> in audition (out 0 (sinOsc AR (mce [x, varLag x 1 x]) 0 * 0.1))

compare to lag UGen
> let x = mouseX KR 220 440 Linear 0.2
> in audition (out 0 (sinOsc AR (mce [x, lag x 1]) 0 * 0.1))
