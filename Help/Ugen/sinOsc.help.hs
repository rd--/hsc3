-- sinOsc ; fixed frequency (hz) and initial-phase (radians)
sinOsc ar (midiCps 69) 0 * 0.1 -- 415 440

-- sinOsc ; control input for frequency ; ie. withSc3 (Sound.OSC.sendMessage (n_set1 (-1) "mnn" 64))
sinOsc ar (midiCps (control kr "mnn" 69)) 0 * 0.25

-- sinOsc ; modulate freq
sinOsc ar (xLine kr 2000 200 1 RemoveSynth) 0 * 0.5

-- sinOsc ; modulate freq
let f1 = xLine kr 1 1000 9 RemoveSynth
    f2 = sinOsc ar f1 0 * 200 + 800 -- (-1,1) ; (-200,200) ; (600,1000)
in sinOsc ar f2 0 * 0.25

-- sinOsc ; modulate phase ; modulator here is in (-2pi,2pi)
let ph = sinOsc ar (xLine kr 20 8000 10 RemoveSynth) 0 * 2 * pi
in sinOsc ar 800 ph * 0.1

-- sinOsc ; phase input only
let ph = sinOsc ar (xLine kr 20 8000 10 RemoveSynth) 0 * pi
in sinOsc ar 0 ph * 0.1

-- sinOsc ; mouse control
let x = mouseX kr 40 10000 Exponential 0.2
    y = mouseY kr 0.01 0.25 Exponential 0.2
in sinOsc ar x 0 * y

-- sinOsc ; simple bell-like tone
let f = mce [0.5,1,1.19,1.56,2,2.51,2.66,3.01,4.1]
    a = mce [0.25,1,0.8,0.5,0.9,0.4,0.3,0.6,0.1]
    o = sinOsc ar (500 * f) 0 * a
    e = envGen kr 1 0.1 0 1 RemoveSynth (envPerc 0.01 10)
in mix o * e

-- sinOsc ; "When two pure tones of slightly different frequency are superposed, our ears
-- perceive audible beats at a rate given by the difference of the two frequencies."
let f0 = 220
    f1 = 221.25
    d = abs (f1 - f0)
    i = impulse ar d 0 * max (sinOsc kr 0.05 0 * 0.1) 0
in sinOsc ar (mce2 f0 f1) 0 * 0.1 + i

-- sinOsc ; "When two tones are sounded together, a tone of lower frequency is
-- frequently heard. Such a tone is called a combination tone.  The most
-- commonly heard combination tone occurs at a frequency f2 - f1."
let f1 = 300
    f2 = 300 * 3/2 {- 450 -}
    f3 = abs (f2 - f1) {- 150 -}
    a3 = max (sinOsc kr 0.05 0 * 0.1) 0
in mix (sinOsc ar (mce3 f1 f2 f3) 0 * mce3 0.1 0.1 a3)

-- sinOsc ; with frequency of zero, operates as table lookup variant of sin
let mk_phasor (l,r) f = phasor ar 0 ((r - l) * f / sampleRate) l r l
    ph = mk_phasor (0,two_pi) 440
    o1 = sinOsc ar 440 0
    o2 = sinOsc ar 0 ph
    o3 = sin ph
in mce2 o1 (xFade2 o2 o3 (lfTri kr 0.1 0) 1) * 0.1

-- sinOsc ; sync ; i.e. <https://www.listarc.bham.ac.uk/lists/sc-dev/msg58316.html>
let dt = mce2 1.0 1.003
    freq = mouseX kr (100 * dt) (3000 * dt) Exponential 0.2
    sync_freq = mouseY kr 100 500 Exponential 0.2
    ph_freq = impulse ar sync_freq 0 + impulse ar freq 0
    o = sinOsc ar 0 (phasor ar ph_freq (freq / sampleRate) 0 1 0 * 2 * pi)
in o * 0.15

-- sinOsc ; reverse sync
let dt = mce2 1.0 1.003
    freq = mouseX kr (100 * dt) (3000 * dt) Exponential 0.2
    sync_freq = mouseY kr 100 500 Exponential 0.2
    direction = toggleFF (impulse ar sync_freq 0) * (-2) + 1
    o = sinOsc ar 0 (wrap (sweep ar (impulse ar freq 0) (direction * freq)) 0 (2 * pi))
in o * 0.15

-- sinOsc ; reverse cycle & reverse sync
let freq = mouseX kr (100 * mce2 1.0 1.003) (3000 * mce2 1.0 1.003) Exponential 0.2
    sync_freq = mouseY kr 100 500 Exponential 0.2
    direction = toggleFF (impulse ar sync_freq 0 + impulse ar freq 0) * (-2) + 1
    o = sinOsc ar 0 (wrap (sweep ar 0 (direction * freq)) 0 (2 * pi))
in o * 0.5

-- sinOsc ; 15.5 khz
pan2 (sinOsc ar 15500 0) 0 0.75

-- sinOsc ; 12 khz - 15.5 khz sweep
pan2 (sinOsc ar (range_hs (12000,15500) (sinOsc kr (1/6) 0)) 0) 0 0.75

-- sinOsc ; fm
let f0 = 220
    cR = 1 -- carrier ratio
    mR = 2 -- modulator ratio
    mA = mouseY kr 0.01 4000 Exponential 0.2 -- moduluation amplitude
in sinOsc ar ((f0 * cR) + (sinOsc ar (f0 * mR) 0 * mA)) 0 * 0.1

-- sinOsc ; fm
let f0 = 220
    cR = 1 -- carrier ratio
    mR = 2 -- modulator ratio
    mI = mouseY kr 0.01 10 Exponential 0.2 -- moduluation index
in sinOsc ar ((f0 * cR) + (sinOsc ar (f0 * mR) 0 * mI * (f0 * mR))) 0 * 0.1

-- sinOsc ; event control
let f (c,g,_,_,z,o,rx,_,p,_,_) =
      let freq_mod = sinOsc kr (tRand 0.02 0.06 g) 0 * tRand 0.1 2 g
          amp_mod = sinOsc kr (tRand 1 3 g / 60) 0 `in_range` (0.2, 0.5)
          s = sinOsc ar (lag (unitCps p) (rx * 2) + freq_mod) (tRand 0 (2 * pi) g)
          l = sinOsc kr o (tRand 0 pi g) * tRand 0.1 0.99 g
      in pan2 s l (lagUD g 0 1 * z * amp_mod)
in mix (voicer 16 f) * control kr "gain" 1.5

-- sinOsc ; event control ; id
let f (c,g,_,_,z,o,rx,_,p,_,_) =
      let freq_mod = sinOsc kr (tRandId (c,'α') 0.02 0.06 g) 0 * tRandId (c,'β') 0.1 2 g
          amp_mod = sinOsc kr (tRandId (c,'γ') 1 3 g / 60) 0 `in_range` (0.2, 0.5)
          s = sinOsc ar (lag (unitCps p) (rx * 2) + freq_mod) (tRandId (c,'δ') 0 (2 * pi) g)
          l = sinOsc kr o (tRandId (c,'ε') 0 pi g) * tRandId (c,'ζ') 0.1 0.99 g
      in pan2 s l (lagUD g 0 1 * z * amp_mod)
in mix (voicer 16 f) * control kr "gain" 1.5

-- sinOsc ; control_m inputs
let f = control_m kr "mnn" 69 (0,127,"lin")
    a = control_m kr "amp" 0.25 (0,1,"amp")
in sinOsc ar (midiCps f) 0 * a

-- sinOsc ; ln 2021-04-05 https://lukasnowok.github.io/spectrology/
let geom k z m = mce (take k (iterate (* m) z))
    o1 = sinOsc ar 10000 0 * geom 4 0.01 8
    o2 = sinOsc ar (lfSaw ar (geom 4 0.05 2) 0 * geom 4 1000 2) 0
in mix (o1 * o2) * 1/4 * 0.1

-- sinOsc ; ln 2021-04-17 https://lukasnowok.github.io/spectrology/
let geom k z m = mce (take k (iterate (* m) z))
    mkenv l t = envGen ar 1 1 0 1 DoNothing (envelope l t [EnvSin,EnvSin])
    o1 = mix (sinOsc ar (geom 10 1000 1.4) 0 * mkenv [1,0,1] [10,10]) * 0.2
    o2 = mix (sinOsc ar (geom 10 800 1.4) 0 * mkenv [0,1,0] [10,10]) * 0.2
    o3 = pulse ar (1/4) 0.5
in (o1 + o2 + o3) * 0.1

-- jh ; <https://scsynth.org/t/auditory-illusion-with-exponentially-spaced-frequencies/4157>
mix (sinOsc ar (300 * mce (map (4 **) (take 200 [0, 0.002 ..]))) 0 * 0.02)

-- sinOsc ; https://scsynth.org/t/what-kind-of-synthesis-is-this/4318/30
let mratio = 4
    amp = 0.2
    boost = 2
    tr = impulse ar (lfNoise2 kr 1 `in_exprange` (1, 100)) 0
    envgen x = let c = EnvNum (-8) in envGen ar tr 1 0 1 DoNothing (envPerc_c 0.0001 x 1 (c,c))
    freq = (midiCps 60.5 / 16) * (linExp (envgen 0.4) 0 1 1 40)
    ix = mouseY kr 0 2 Linear 0.1
    sig = sinOsc ar freq (sinOsc ar (freq * mratio) 0 * ix)
in tanh (pan2 sig 0 boost) * envgen 1 * amp

---- ; drawings
Ui.ui_baudline (4096 * 1) 50 "linear" 2
Sound.Sc3.Plot.plot_ugen_nrt (48000,64) 1.0 (sinOsc ar 1 0)
(midiCps 60.5 / 16)
