> Sound.SC3.UGen.Help.viewSC3Help "Dseq"
> Sound.SC3.UGen.DB.ugenSummary "Dseq"
# inputReordering: [1,0]

> import Sound.SC3.ID

> let {n = dseq 'α' 3 (mce [1, 3, 2, 7, 8])
>     ;x = mouseX KR 1 40 Exponential 0.1
>     ;t = impulse KR x 0
>     ;f = demand t 0 n * 30 + 340}
> in audition (out 0 (sinOsc AR f 0 * 0.1))

At audio rate.
> let {n = dseq 'α' dinf (mce [1,3,2,7,8,32,16,18,12,24])
>     ;x = mouseX KR 1 10000 Exponential 0.1
>     ;t = impulse AR x 0
>     ;f = demand t 0 n * 30 + 340}
> in audition (out 0 (sinOsc AR f 0 * 0.1))

The SC2 Sequencer UGen is somewhat like the sequ function below
> let {sequ e s tr = demand tr 0 (dseq e dinf (mce s))
>     ;t = lfPulse AR 6 0 0.5
>     ;n0 = sequ 'α' [60,62,63,58,48,55] t
>     ;n1 = sequ 'β' [63,60,48,62,55,58] t
>     ;o = lfSaw AR (midiCPS (mce2 n0 n1)) 0 * 0.1}
> in audition (out 0 o)
