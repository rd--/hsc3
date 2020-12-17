-- diodeRingMod
let o1 = sinOsc AR 440 0
    o2 = sinOsc AR (xLine KR 1 100 10 DoNothing) 0
in mce2 (X.diodeRingMod o1 o2) (o1 * o2) * 0.125

-- diodeRingMod
let o1 = sinOsc AR 440 0
    o2 = sinOsc AR (xLine KR 200 500 5 DoNothing) 0
in mce2 (X.diodeRingMod o1 o2) (o1 * o2) * 0.125

-- diodeRingMod
let s1 = sinOsc AR (3700 * mce [1, 1.1, 1.2] * range 1 2 (sinOsc AR 200 0)) 0
    s2 = sinOsc AR (100 * mce [0.75, 1, 0.5]) 0
    s3 = X.diodeRingMod s1 s2
in mix s3 * lfPulse AR (10.3 * 0.5) 0 0.04 * 0.1

-- diodeRingMod
let mf_sin = sinOsc AR
    mf_square f _ = X.blitB3Square AR f 0.99
    mf_ring_mod (lfo_ph,car_ph) car_freq lfo_type lfo_freq lfo_amp drive x_mix mod_sig =
      let range_2oct = range 0.25 2
          lfo = range_2oct (lfo_type lfo_freq lfo_ph * lfo_amp)
          car_sig = sinOsc AR (car_freq * lfo) car_ph
          mod_sig_post = mod_sig * drive
      in xFade2 mod_sig_post (X.diodeRingMod car_sig mod_sig_post) x_mix 1
    y = mouseY KR 0 1 Linear 0.2
    x = mouseX KR (-1) 1 Linear 0.2
in mf_ring_mod (0,0) 6.25 mf_sin 0.1 y 1 x (soundIn 0)

{---- ; moog

c_freq = carrier frequency (mf = six-octave range, 2-130 and 60-4000 hz)
lfo_type = LFO signal function, ie. mf_sin or mf_square
lfo_freq = LFO frequency (mf = 0.1 - 25.0 hz)
lfo_amp = the amount that the LFO output sweeps the carrier sin oscillator
drive = pre-multiplier for mod_sig
x_mix = crossfade from unmodulated to modulated audio (-1 to 1)

-}
