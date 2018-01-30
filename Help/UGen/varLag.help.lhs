    Sound.SC3.UGen.Help.viewSC3Help "VarLag"
    Sound.SC3.UGen.DB.ugenSummary "VarLag"

see also: varLag_env

> import Sound.SC3 {- hsc3 -}

> exprange l r s = linExp s (-1) 1 l r

> varLag_env in_ time curve start =
>   let rt = rateOf in_
>       e = Envelope [start,in_] [time] [curve] Nothing Nothing 0
>       time_ch = if rateOf time == IR then 0 else changed time 0
>       tr = changed in_ 0 + time_ch + impulse rt 0 0
>   in envGen rt tr 1 0 1 DoNothing e

> f_01 f = sinOsc AR (f (exprange 200 400 (lfNoise1 'α' KR 5)) 0.1) 0 * 0.2

> g_01 = f_01 lag

> g_02 = f_01 (\s t -> varLag_env s t (EnvNum 0) s)

> f_02 f =
>     let x = mouseX KR 220 440 Linear 0.2
>     in sinOsc AR (mce [x, f x 1]) 0 * 0.1

> g_03 = f_02 lag

> g_04 = f_02 (\s t -> varLag_env s t (EnvNum 0) s)

> g_05 =
>   let fr = range 100 400 (lfPulse KR 1 0 0.5) -- frequency modulator
>       sh = line KR (-8) 8 15 RemoveSynth -- modulate shape
>       fr_lag = varLag fr 0.2 sh 5 0 -- lag the modulator
>   in sinOsc AR fr_lag 0 * 0.3

as signal filter (step behaviour is wrong?)

> f_03 s =
>   let x = mouseX KR 0.0001 0.01 Exponential 0.2
>       y = mouseY KR (-8) 8 Linear 0.2
>   in varLag s x y 5 0

> g_06 = f_03 (0 - saw AR 440) * 0.15
> g_07 = f_03 (impulse AR (range 6 24 (lfNoise2 'γ' KR 4)) 0) * 0.5

> g_08 =
>   let s = varSaw AR 220 0 (range 0 1 (sinOsc KR 0.25 0))
>   in f_03 s * 0.1
