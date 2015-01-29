> Sound.SC3.UGen.Help.viewSC3Help "TDuty"
> Sound.SC3.UGen.DB.ugenSummary "TDuty"

> import Sound.SC3

Play a little rhythm

> let d = dseq 'α' dinf (mce [0.1, 0.2, 0.4, 0.3])
> in audition (out 0 (tDuty AR d 0 DoNothing 1 0))

Amplitude changes

> let {d0 = dseq 'α' dinf (mce [0.1, 0.2, 0.4, 0.3])
>     ;d1 = dseq 'β' dinf (mce [0.1, 0.4, 0.01, 0.5, 1.0])
>     ;s = ringz (tDuty AR d0 0 DoNothing d1 1) 1000 0.1}
> in audition (out 0 s)

Mouse control.

> let {d = dseq 'α' dinf (mce [0.1, 0.4, 0.01, 0.5, 1.0])
>     ;x = mouseX KR 0.003 1 Exponential 0.1
>     ;s = ringz (tDuty AR x 0 DoNothing d 1) 1000 0.1 * 0.5}
> in audition (out 0 s)

Note that the 440 is the shorter pitch, since gap is set to false

> let {d0 = dser 'α' 12 (mce [0.1, 0.3])
>     ;d1 = dser 'β' 12 (mce [440, 880])
>     ;t = tDuty AR d0 0 RemoveSynth d1 0}
> in audition (out 0 (sinOsc AR (latch t t) 0 * 0.1))

Abstraction

> import Data.List

> let {bp n d act = let {(e,t) = unzip d
>                       ;mk z l = dser z n (mce l)
>                       ;sq = tDuty AR (mk 'α' t) 0 act (mk 'β' e) 0}
>                   in latch sq sq
>     ;bp' d = bp (genericLength d) d
>     ;tm m = let f (e,t) = (e,t * m) in map f
>     ;f1 = midiCPS (bp 35 (tm 0.125 [(60,1),(63,1),(67,2),(68,1),(62,1)]) RemoveSynth)
>     ;f2 = midiCPS (bp' [(60,1),(63,0.5),(67,0.5),(68,1),(62,1)] DoNothing)}
> in audition (out 0 (sinOsc AR (mce2 f1 f2) 0 * 0.1))
