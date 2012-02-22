> Sound.SC3.UGen.Help.viewSC3Help "GbmanN"
> Sound.SC3.UGen.DB.ugenSummary "GbmanN"

> import Sound.SC3

default initial params
> let x = mouseX KR 20 sampleRate Linear 0.2
> in audition (out 0 (gbmanN AR x 1.2 2.1 * 0.1))

change initial params
> let x = mouseX KR 20 sampleRate Linear 0.2
> in audition (out 0 (gbmanN AR x (-0.7) (-2.7) * 0.1))

wait for it...
> let x = mouseX KR 20 sampleRate Linear 0.2
> in audition (out 0 (gbmanN AR x 1.2 2.0002 * 0.1))

as a frequency control
> let f = gbmanN AR 40 1.2 2.1 * 400 + 500
> in audition (out 0 (sinOsc AR f 0 * 0.4))
