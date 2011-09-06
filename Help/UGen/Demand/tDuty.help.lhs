> Sound.SC3.UGen.Help.viewSC3Help "TDuty"
> Sound.SC3.UGen.DB.ugenSummary "TDuty"

> import Sound.SC3.ID

Play a little rhythm
> let d = dseq 'a' dinf (mce [0.1, 0.2, 0.4, 0.3])
> in audition (out 0 (tDuty AR d 0 DoNothing 1 0))

Amplitude changes
> let {d0 = dseq '0' dinf (mce [0.1, 0.2, 0.4, 0.3])
>     ;d1 = dseq '1' dinf (mce [0.1, 0.4, 0.01, 0.5, 1.0])
>     ;s = ringz (tDuty AR d0 0 DoNothing d1 1) 1000 0.1}
> in audition (out 0 s)

Mouse control.
> let {d = dseq 'a' dinf (mce [0.1, 0.4, 0.01, 0.5, 1.0])
>     ;x = mouseX' KR 0.003 1 Exponential 0.1
>     ;s = ringz (tDuty AR x 0 DoNothing d 1) 1000 0.1 * 0.5}
> in audition (out 0 s)

Note that the 440 is the shorter pitch, since gap is set to false
> let {d0 = dser '0' 12 (mce [0.1, 0.3])
>     ;d1 = dser '1' 12 (mce [440, 880])
>     ;t = tDuty AR d0 0 RemoveSynth d1 0}
> in audition (out 0 (sinOsc AR (latch t t) 0 * 0.1))
