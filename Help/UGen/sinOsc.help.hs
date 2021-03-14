-- sinOsc ; fixed frequency (hz) and initial-phase (radians)
sinOsc AR (midiCPS 69) 0 * 0.1 -- 415 440

-- sinOsc ; control input for frequency ; ie. withSC3 (Sound.OSC.sendMessage (n_set1 (-1) "mnn" 64))
sinOsc AR (midiCPS (control KR "mnn" 69)) 0 * 0.25

-- sinOsc ; modulate freq
sinOsc AR (xLine KR 2000 200 1 RemoveSynth) 0 * 0.5

-- sinOsc ; modulate freq
let f1 = xLine KR 1 1000 9 RemoveSynth
    f2 = sinOsc AR f1 0 * 200 + 800 -- (-1,1) ; (-200,200) ; (600,1000)
in sinOsc AR f2 0 * 0.25

-- sinOsc ; Modulate phase
let ph = sinOsc AR (xLine KR 20 8000 10 RemoveSynth) 0 * 2 * pi
in sinOsc AR 800 ph * 0.1

-- sinOsc ; mouse control
let x = mouseX KR 40 10000 Exponential 0.2
    y = mouseY KR 0.01 0.25 Exponential 0.2
in sinOsc AR x 0 * y

-- sinOsc ; simple bell-like tone
let f = mce [0.5,1,1.19,1.56,2,2.51,2.66,3.01,4.1]
    a = mce [0.25,1,0.8,0.5,0.9,0.4,0.3,0.6,0.1]
    o = sinOsc AR (500 * f) 0 * a
    e = envGen KR 1 0.1 0 1 RemoveSynth (envPerc 0.01 10)
in mix o * e

-- sinOsc ; "When two pure tones of slightly different frequency are superposed, our ears
-- perceive audible beats at a rate given by the difference of the two frequencies."
let f0 = 220
    f1 = 221.25
    d = abs (f1 - f0)
    i = impulse AR d 0 * max (sinOsc KR 0.05 0 * 0.1) 0
in sinOsc AR (mce2 f0 f1) 0 * 0.1 + i

-- sinOsc ; "When two tones are sounded together, a tone of lower frequency is
-- frequently heard. Such a tone is called a combination tone.  The most
-- commonly heard combination tone occurs at a frequency f2 - f1."
let f1 = 300
    f2 = 300 * 3/2 {- 450 -}
    f3 = abs (f2 - f1) {- 150 -}
    a3 = max (sinOsc KR 0.05 0 * 0.1) 0
in mix (sinOsc AR (mce3 f1 f2 f3) 0 * mce3 0.1 0.1 a3)

-- sinOsc ; with frequency of zero, operates as table lookup variant of sin
let mk_phasor (l,r) f = phasor AR 0 ((r - l) * f / sampleRate) l r l
    ph = mk_phasor (0,two_pi) 440
    o1 = sinOsc AR 440 0
    o2 = sinOsc AR 0 ph
    o3 = sin ph
in mce2 o1 (xFade2 o2 o3 (lfTri KR 0.1 0) 1) * 0.1

-- sinOsc ; sync ; i.e. <https://www.listarc.bham.ac.uk/lists/sc-dev/msg58316.html>
let dt = mce2 1.0 1.003
    freq = mouseX KR (100 * dt) (3000 * dt) Exponential 0.2
    sync_freq = mouseY KR 100 500 Exponential 0.2
    ph_freq = impulse AR sync_freq 0 + impulse AR freq 0
    o = sinOsc AR 0 (phasor AR ph_freq (freq / sampleRate) 0 1 0 * 2 * pi)
in o * 0.15

-- sinOsc ; reverse sync
let dt = mce2 1.0 1.003
    freq = mouseX KR (100 * dt) (3000 * dt) Exponential 0.2
    sync_freq = mouseY KR 100 500 Exponential 0.2
    direction = toggleFF (impulse AR sync_freq 0) * (-2) + 1
    o = sinOsc AR 0 (wrap (sweep (impulse AR freq 0) (direction * freq)) 0 (2 * pi))
in o * 0.15

-- sinOsc ; reverse cycle & reverse sync
let freq = mouseX KR (100 * mce2 1.0 1.003) (3000 * mce2 1.0 1.003) Exponential 0.2
    sync_freq = mouseY KR 100 500 Exponential 0.2
    direction = toggleFF (impulse AR sync_freq 0 + impulse AR freq 0) * (-2) + 1
    o = sinOsc AR 0 (wrap (sweep (k2a 0) (direction * freq)) 0 (2 * pi))
in o * 0.5

-- sinOsc ; 15.5 khz
pan2 (sinOsc AR 15500 0) 0 0.75

-- sinOsc ; 12 khz - 15.5 khz sweep
pan2 (sinOsc AR (range_hs (12000,15500) (sinOsc KR (1/6) 0)) 0) 0 0.75

-- sinOsc ; fm
let f0 = 220
    cR = 1 -- carrier ratio
    mR = 2 -- modulator ratio
    mA = mouseY KR 0.01 4000 Exponential 0.2 -- moduluation amplitude
in sinOsc AR ((f0 * cR) + (sinOsc AR (f0 * mR) 0 * mA)) 0 * 0.1

-- sinOsc ; fm
let f0 = 220
    cR = 1 -- carrier ratio
    mR = 2 -- modulator ratio
    mI = mouseY KR 0.01 10 Exponential 0.2 -- moduluation index
in sinOsc AR ((f0 * cR) + (sinOsc AR (f0 * mR) 0 * mI * (f0 * mR))) 0 * 0.1

-- sinOsc ; event control
let f c (g,_,_,z,o,rx,_,p,_,_) =
      let freq_mod = sinOsc KR (tRand (c,'α') 0.02 0.06 g) 0 * tRand (c,'β') 0.1 2 g
          amp_mod = sinOsc KR (tRand (c,'γ') 1 3 g / 60) 0 `in_range` (0.2, 0.5)
          s = sinOsc AR (lag (midiCPS p) (rx * 2) + freq_mod) (tRand (c,'δ') 0 (2 * pi) g)
          l = sinOsc KR o (tRand (c,'ε') 0 pi g) * tRand (c,'ζ') 0.1 0.99 g
      in pan2 s l (lagUD g 0 1 * z * amp_mod)
in mix (rEventVoicer 16 f) * control KR "gain" 1.5

-- sinOsc ; control_md inputs
let f = control_md KR "mnn" 69 (0,127,"lin",1,"")
    a = control_md KR "amp" 0.25 (0,4,"amp",0,"*")
in sinOsc AR (midiCPS f) 0 * a

---- ; drawings
Sound.SC3.Plot.plot_ugen_nrt (48000,64) 1.0 (sinOsc AR 1 0)
