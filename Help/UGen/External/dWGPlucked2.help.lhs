> Sound.SC3.UGen.Help.viewSC3Help "DWGPlucked2"
> Sound.SC3.UGen.DB.ugenSummary "DWGPlucked2"

> import Sound.SC3

self deleting

> let {amp = 0.5
>     ;gate = 1
>     ;freq = 440
>     ;c3 = 20
>     ;pan = 0
>     ;e = Envelope
>      [0,1,1,0] [0.001,0.006,0.0005]
>      (map EnvNum [5,-5,-8]) Nothing Nothing
>     ;i = amp * lfClipNoise 'α' AR 2000 * envGen AR gate 1 0 1 DoNothing e
>     ;s = dWGPlucked2 AR freq amp gate 0.1 1 c3 i 0.1 1.008 0.55 0.01
>     ;z = detectSilence s 0.001 0.1 RemoveSynth}
> in audition (out 0 (mrg2 (pan2 s pan 0.1) z))

re-sounding

> let {sequ e s tr = demand tr 0 (dseq e dinf (mce s))
>     ;d = dseq 'α' dinf (mce [1,1,2,1,1,1,2,3,1,1,1,1,2,3,4] * 0.175)
>     ;t = tDuty AR d 0 DoNothing 1 0
>     ;amp = tRand 'β' 0.01 0.25 t
>     ;n0 = sequ 'γ' [60,62,63,58,48,55] t
>     ;n1 = sequ 'δ' [63,60,48,62,55,58] t
>     ;freq = midiCPS (mce2 n0 n1)
>     ;c3 = tRand 'ε' 300 1400 t
>     ;pan = tRand 'ζ' (-1) 1 t
>     ;e_dt = tRand 'η' 0.05 0.150 t
>     ;mt = tRand 'θ' 0.992 1.008 t
>     ;pp = tRand 'ι' 0.05 0.15 t
>     ;dt = tRand 'κ' 0.25 1.75 t
>     ;env = decay2 t 0.001 e_dt * lfClipNoise 'λ' AR 2000
>     ;i = amp * lfClipNoise 'μ' AR 2000 * env
>     ;ps = dWGPlucked2 AR freq amp 1 pp (1 / dt) c3 i 0.1 mt 0.55 0.01}
> in audition (out 0 (pan2 ps pan 0.1))
