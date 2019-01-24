    Sound.SC3.UGen.Help.viewSC3Help "DWGPlucked2"
    Sound.SC3.UGen.DB.ugenSummary "DWGPlucked2"

> import Sound.SC3
> import qualified Sound.SC3.UGen.Bindings.DB.External as X {- hsc3 -}

self deleting

> g_01 =
>   let freq = 440
>       amp = 0.5
>       gate_ = 1
>       c3 = 20
>       inp = let e = envelope [0,1,1,0] [0.001,0.006,0.0005] (map EnvNum [5,-5,-8])
>             in amp * lfClipNoise 'α' AR 2000 * envGen AR gate_ 1 0 1 DoNothing e
>       ps = X.dWGPlucked2 AR freq amp gate_ 0.1 1 c3 inp 0.1 1.008 0.55 0.01
>       pan = 0
>       z = detectSilence ps 0.001 0.1 RemoveSynth
>   in mrg2 (pan2 ps pan 0.1) z

re-sounding

> f_02 dur =
>   let sequ e s tr = demand tr 0 (dseq e dinf (mce s))
>       t = let d = dseq 'α' dinf dur
>           in tDuty AR d 0 DoNothing 1 0
>       freq = let n0 = sequ 'β' [60,62,63,58,48,55] t
>                  n1 = sequ 'γ' [63,60,48,62,55,58] t
>              in midiCPS (mce2 n0 n1)
>       amp = tRand 'δ' 0.01 0.35 t -- pulse amplitude (0  - 1, def = 0.5)
>       gate_ = 1 -- synth release
>       pos = tRand 'ε' 0.05 0.25 t -- pluck position (0 - 1, def = 0.14)
>       c1 = 1 / tRand 'ζ' 0.25 1.75 t -- reciprocal of decay time (def = 1.0)
>       c3 = tRand 'η' 10 1400 t -- high frequency loss factor (def = 30)
>       inp = let e_dt = tRand 'θ' 0.05 0.150 t
>                 env = decay2 t 0.001 e_dt * lfClipNoise 'ι' AR 2000
>             in amp * lfClipNoise 'κ' AR 2000 * env -- pluck signal
>       release = tRand 'λ' 0.05 0.15 t -- release time (seconds, def = 0.1)
>       mistune = tRand 'μ' 0.992 1.008 t -- factor for detuning second string (def = 1.008)
>       mp = tRand 'ν' 0.35 0.65 t -- exitation mixer (def = 0.55)
>       gc = tRand 'ξ' 0.001 0.020 t -- coupling string factor (def = 0.01)
>       ps = X.dWGPlucked2 AR freq amp gate_ pos c1 c3 inp release mistune mp gc
>       pan = tRand 'ο' (-1) 1 t
>   in pan2 ps pan 0.1

> g_02 = f_02 (mce [1,1,2,1,1,1,2,3,1,1,1,1,2,3,4] * 0.175)

and scaling

> g_03 =
>   let m = mouseX KR 0.25 2.0 Linear 0.2
>       d = 1 / (2 ** roundE (mce [1,1,2,1,1,1,2,3,1,1,1,1,2,3,4]))
>   in f_02 (d * m)
