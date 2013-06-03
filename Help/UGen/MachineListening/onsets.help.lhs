> Sound.SC3.UGen.Help.viewSC3Help "Onsets"
> Sound.SC3.UGen.DB.ugenSummary "Onsets"

> import Sound.SC3.ID

allocate buffer 10
> withSC3 (async (b_alloc 10 512 1))

> let { x = mouseX KR 0 1 Linear 0.2
>     ; i = soundIn 4
>     ; c = fft' 10 i
>     ; o = onsets' c x (onsetType "rcomplex")
>     ; s = sinOsc AR 440 0 * 0.2
>     ; e = envGen KR o 1 0 1 DoNothing (envPerc 0.001 0.1) }
> in audition (out 0 (s * e))

> audition (out 0 (soundIn 0 * 0.1))

a generative signal with distinct onsets!
> let z = let {e = linLin (saw AR 2) (-1) 1 0 1
>             ;p = let f = midiCPS (tIRand 'a' 63 75 (impulse KR 2 0))
>                  in pulse AR f 0.5
>             ;f = linExp (lfNoise2 'a' KR 0.5) (-1) 1 100 10000}
>         in lpf p f * e

> audition (out 0 z)

x varies threshold, whitenoise bursts indicate detected onsets
> let {c = fft' 10 z
>     ;x = mouseX KR 0 1 Linear 0.2
>     ;o = onsets' c x (onsetType "rcomplex")
>     ;p = let d = envPerc 0.001 0.1
>          in whiteNoise 'a' AR * envGen KR o 0.2 0 1 DoNothing d}
> in audition (out 0 (pan2 z (-0.75) 0.2 + pan2 p 0.75 1))
