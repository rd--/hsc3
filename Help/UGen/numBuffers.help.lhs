> Sound.SC3.UGen.Help.viewSC3Help "NumBuffers"
> Sound.SC3.UGen.DB.ugenSummary "NumBuffers"

> import Sound.SC3

the number of audio buffers available at the server (by default 1024)
> audition (poll (impulse KR 1 0) numBuffers (label "numBuffers") 0)

> let f = 110 + numBuffers
> in audition (out 0 (sinOsc AR f 0 * 0.1))
