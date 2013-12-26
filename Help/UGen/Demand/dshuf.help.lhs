> Sound.SC3.UGen.Help.viewSC3Help "Dshuf"
> Sound.SC3.UGen.DB.ugenSummary "Dshuf"

# sclang re-orders inputs

> import Sound.SC3.ID {- hsc3 -}

> let {a = dseq 'α' dinf (dshuf 'β' 3 (mce [1,3,2,7,8.5]))
>     ;x = mouseX KR 1 40 Exponential 0.1
>     ;t = impulse KR x 0
>     ;f = demand t 0 a * 30 + 340}
> in audition (out 0 (sinOsc AR f 0 * 0.1))

> import Sound.SC3.UGen.External.RDU.ID {- sc3-rdu -}

> let {a = dseq 'α' dinf (dshuf 'β' 5 (randN 81 'γ' 0 10))
>     ;x = mouseX KR 1 10000 Exponential 0.1
>     ;t = impulse AR x 0
>     ;f = demand t 0 a * 30 + 340}
> in audition (out 0 (sinOsc AR f 0 * 0.1))
