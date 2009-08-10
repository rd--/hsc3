pmOsc rate cf mf pm mp

phase modulation oscillator (composite UGen)

    cf = carrier frequency
    mf = modulation frequency
    pm = modulator amplitude
    mp = modulator phase

The definition is:

  pmOsc r cf mf pm mp = sinOsc r cf (sinOsc r mf mp * pm)

> import Sound.SC3

> do { cf <- rand 0 2000
>    ; mf <- rand 0 800
>    ; pm' <- rand 0 12
>    ; l <- rand (-1) 1
>    ; let { t = envLinen 2 5 2 1 [EnvLin, EnvLin, EnvLin]
>          ; e = envGen KR 1 0.1 0 1 RemoveSynth t
>          ; pm = line KR 0 pm' 9 DoNothing }
>      in audition (out 0 (linPan2 (pmOsc AR cf mf pm 0) l e)) }
