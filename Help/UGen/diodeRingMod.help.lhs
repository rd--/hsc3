    Sound.SC3.UGen.Help.viewSC3Help "DiodeRingMod"
    Sound.SC3.UGen.DB.ugenSummary "DiodeRingMod"

> import Sound.SC3 {- hsc3 -}
> import Sound.SC3.UGen.Bindings.DB.External {- hsc3 -}

> o_01 = sinOsc AR 440 0
> o_02 = sinOsc AR (xLine KR 1 100 10 DoNothing) 0
> o_03 = sinOsc AR (xLine KR 200 500 5 DoNothing) 0
> g_01 = diodeRingMod o_01 o_02 * 0.125
> g_02 = (o_01 * o_02) * 0.125
> g_03 = diodeRingMod o_01 o_03 * 0.125
> g_04 = (o_01 * o_03) * 0.125

> g_05 =
>   let s1 = sinOsc AR (3700 * mce [1, 1.1, 1.2] * range 1 2 (sinOsc AR 200 0)) 0
>       s2 = sinOsc AR (100 * mce [0.75, 1, 0.5]) 0
>       s3 = diodeRingMod s1 s2
>   in mix s3 * lfPulse AR (10.3 * 0.5) 0 0.04 * 0.1

> mf_sin f = sinOsc AR f 0

> mf_square f = blitB3Square AR f 0.99

c_freq = carrier frequency (mf = six-octave range, 2-130 and 60-4000 hz)

lfo_type = LFO signal function, ie. mf_sin or mf_square

lfo_freq = LFO frequency (mf = 0.1 - 25.0 hz)

lfo_amp = the amount that the LFO output sweeps the carrier sin oscillator

drive = pre-multiplier for mod_sig

x_mix = crossfade from unmodulated to modulated audio (-1 to 1)

> mf_ring_mod c_freq lfo_type lfo_freq lfo_amp drive x_mix mod_sig =
>   let range_2oct = range 0.25 2
>       lfo = range_2oct (lfo_type lfo_freq * lfo_amp)
>       car_sig = sinOsc AR (c_freq * lfo) 0
>       mod_sig_post = mod_sig * drive
>   in xFade2 mod_sig_post (diodeRingMod car_sig mod_sig_post) x_mix 1

> g_07 =
>   let c_freq = 6.25
>       lfo_freq = 0.1
>       lfo_amp = mouseY KR 0 1 Linear 0.2
>       drive = 1
>       x_mix = mouseX KR (-1) 1 Linear 0.2
>       mod_sig = soundIn 0
>   in mf_ring_mod c_freq mf_sin lfo_freq lfo_amp drive x_mix mod_sig
