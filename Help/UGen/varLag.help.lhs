> import Sound.SC3 {- hsc3 -}

The implemented varLag UGen has three inputs: (input, lagTime, start)

> g_00 = varLag (lfPulse AR 50 0 0.5) (mouseX KR 0.0 (1/50) Linear 0.2) 0 * 0.2

> g_01 = varLag (impulse AR 50 0) (mouseX KR 0.0 (1/50) Linear 0.2) 0 * 0.2

The varLag_env composite UGen has various odd behaviours

> f_01 f = sinOsc AR (f (exprange 200 400 (lfNoise1 'α' KR 5)) 0.1) 0 * 0.2

> g_02 = f_01 lag

> g_03 = f_01 (\s t -> varLag_env s t (EnvNum 0) s)

> f_02 f =
>     let x = mouseX KR 220 440 Linear 0.2
>     in sinOsc AR (mce [x, f x 1]) 0 * 0.1

> g_04 = f_02 lag

> g_05 = f_02 (\s t -> varLag_env s t (EnvNum 0) s)

> g_06 =
>   let fr = range 100 400 (lfPulse KR 1 0 0.5) -- frequency modulator
>       sh = line KR (-8) 8 15 RemoveSynth -- modulate shape
>       fr_lag = varLag_env fr 0.2 (EnvNum sh) 0 -- lag the modulator
>   in sinOsc AR fr_lag 0 * 0.3

as signal filter (step behaviour is wrong?)

> f_03 s =
>   let x = mouseX KR 0.0001 0.01 Exponential 0.2
>   in varLag s x s

> g_07 = f_03 (0 - saw AR 440) * 0.15
> g_08 = f_03 (impulse AR (range 6 24 (lfNoise2 'γ' KR 4)) 0) * 0.5

> g_09 =
>   let s = varSaw AR 220 0 (range 0 1 (sinOsc KR 0.25 0))
>   in f_03 s * 0.1
