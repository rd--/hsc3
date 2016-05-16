> Sound.SC3.UGen.Help.viewSC3Help "FreqShift"
> Sound.SC3.UGen.DB.ugenSummary "FreqShift"

> import Sound.SC3 {- hcs3 -}

Shifting a 100Hz tone by 1 Hz rising to 500Hz

> let {i = sinOsc AR 100 0
>     ;s = xLine KR 1 500 5 RemoveSynth}
> in audition (out 0 (freqShift i s 0 * 0.1))

Shifting a complex tone by 1 Hz rising to 500Hz

> let {d = klangSpec [101, 303, 606, 808] [1, 1, 1, 1] [1, 1, 1, 1]
>     ;i = klang AR 1 0 d
>     ;s = xLine KR 1 500 5 RemoveSynth}
> in audition (out 0 (freqShift i s 0 * 0.1))

Modulating shift and phase

> let {s = lfNoise2 'α' AR 0.3
>     ;i = sinOsc AR 10 0
>     ;p = linLin (sinOsc AR 500 0) (-1) 1 0 (2 * pi)}
> in audition (out 0 (freqShift i (s * 1500) p * 0.1))

Shifting bandpassed noise

> let {n1 = whiteNoise 'α' AR
>     ;n2 = lfNoise0 'β' AR 5.5
>     ;i = bpf n1 1000 0.001
>     ;s = n2 * 1000}
> in audition (out 0 (freqShift i s 0 * 32))

{a=Blip.ar(60,4,LFGauss.ar(4,1/8))
;a=a/4+LocalIn.ar(2)
;a=FreqShift.ar(a,LFNoise0.kr(1/4,90))
;LocalOut.ar(DelayC.ar(a,1,0.1,0.9))
;a}.play

> let {e = lfGauss AR 4 (1/8) 0 Loop DoNothing
>     ;o = blip AR 60 4 * e
>     ;a = o / 4 + localIn 2 AR 0
>     ;s = freqShift a (lfNoise0 'α' KR (1/4) * 90) 0
>     ;z = delayC s 1 0.1 * 0.9}
> in audition (mrg2 (out 0 s) (localOut z))
