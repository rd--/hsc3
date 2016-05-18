    > Sound.SC3.UGen.Help.viewSC3Help "VarLag"
    > Sound.SC3.UGen.DB.ugenSummary "VarLag"

Note: VarLag at sclang is a composite UGen, at hsc3 it's a direct
binding to the underlying UGen.

> import Sound.SC3

used to lag pitch

> g_01 =
>     let x = mouseX KR 220 440 Linear 0.2
>     in sinOsc AR (mce [x, varLag x 1 0 5 x]) 0 * 0.1

compare to lag UGen

> g_02 =
>     let x = mouseX KR 220 440 Linear 0.2
>     in sinOsc AR (mce [x, lag x 1]) 0 * 0.1
