    > Sound.SC3.UGen.Help.viewSC3Help "NumBuffers"
    > Sound.SC3.UGen.DB.ugenSummary "NumBuffers"

> import Sound.SC3 {- hsc3 -}

the number of audio buffers available at the server (by default 1024)

> g_01 = poll (impulse KR 1 0) numBuffers (label "numBuffers") 0
>
> g_02 = let f = 110 + numBuffers in sinOsc AR f 0 * 0.1
