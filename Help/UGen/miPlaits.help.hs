-- MiPlaits ; basics ; 0=virtual_analog
X.miPlaits AR 35 (X.miPlaits_mode "virtual_analog") 0.252 0.5 0.2 0 0 0 0 0 0.5 0.5 * 0.1

-- MiPlaits ; Mouse control ; 2=fm
let harm = mouseX KR 0 1 Linear 0.2
    timb = mouseY KR 0 1 Linear 0.2
in X.miPlaits AR 48 (X.miPlaits_mode "fm") harm timb 0.5 0 0 0 0 0 0.5 0.5 * 0.1

-- MiPlaits ; some talking ; 7=speech
let ptch = mouseY KR 24 72 Linear 0.2
    harm = mouseX KR 0 1 Linear 0.2
    mrph = lfSaw AR 0.11 0 * 0.5 + 0.5
in X.miPlaits AR ptch (X.miPlaits_mode "speech") harm 0.5 mrph 0 0 0 0 0 0.5 0.5 * 0.1

-- MiPlaits ; scanning through some engines + vibrato
let pit = 50
    vib = sinOsc AR 6 0 * 0.3
    eng = lfNoise0 'α' KR 0.5 * 4 + 4
in X.miPlaits AR (pit + vib) eng 0.1 0.5 0.5 0 0 0 0 0 0.5 0.5 * 0.1

-- MiPlaits ; some FM
let harm = mouseX KR 0 1 Linear 0.2
    timbre = mouseY KR 0 1 Linear 0.2
    morph = range 0 1 (lfNoise2 'α' KR 0.2)
    trigger = impulse AR 8 0
in X.miPlaits AR 48 (X.miPlaits_mode "fm") harm timbre morph trigger 0 0 0 0 0.5 0.5 * 0.1

-- MiPlaits ; 5=wavetable
let trigger = impulse KR 8 0
    harm = lfNoise2 'α' KR 0.3 * 0.25 + 0.25
    timbre = lfNoise1 'α' KR 0.4 * 0.5 + 0.5
    dec = range 0.01 0.8 (lfNoise2 'α' KR 0.2)
in X.miPlaits AR 44 (X.miPlaits_mode "wavetable") harm timbre 0 trigger 0 0 0 0 dec 0.5 * 0.2

-- MiPlaits ; 6=chord
let harm = mouseY KR 0 1 Linear 0.2 -- select chord
    trigger = impulse KR 6 0
    lev = tRand 'α' 0 1 trigger
    c0 = head . mceChannels
in c0 (X.miPlaits AR 40 (X.miPlaits_mode "chord") harm 0.5 0.5 trigger (lev * lev) 0 0.4 0 0.5 0.5) * 0.1

-- MiPlaits ; 8=swarm ; super saw
let pit = tiRand 'α' 39 49 (dust 'β' KR 0.5)
    c0 = head . mceChannels
in c0 (X.miPlaits AR pit (X.miPlaits_mode "swarm") 0.16 0 1 0 0 0 0 0 0.5 0.5) * 0.1

-- MiPlaits ; slow swarm
X.miPlaits AR 48 (X.miPlaits_mode "swarm") 0.9 0 0.5 0 0 0 0 0 0.5 0.5 * mce2 0.04 0.15

-- MiPlaits ; 11=string ; inharmonic
let harm = range 0 1 (lfNoise2 'α' KR 0.2)
    c0 = head . mceChannels
in c0 (X.miPlaits AR 60 (X.miPlaits_mode "string") harm 0 0.85 0 0 0 0 0 0.5 0.5)

-- MiPlaits ; 12=modal
let harm = range 0 1 (lfNoise2 'α' KR 0.2)
    trigger = impulse KR 1 0
    lev = squared (tRand 'β' 0 1 trigger)
in mix (X.miPlaits AR 56 (X.miPlaits_mode "modal") harm 0 0.85 trigger lev 0 0 0 0.5 0.5)

-- MiPlaits ; snare drums
let harm = range 0 0.5 (lfNoise2 'α' KR 0.5)
    timbre = 0.173
    morph = 0.827
    trigger = impulse KR 8 0
    lev = squared (tRand 'β' 0 1 trigger)
in X.miPlaits AR 47 14 harm timbre morph trigger lev 0.2 0.43 0 0.4 0.5 * 0.1

-- MiPlaits ; filtered noise
let harm = 0.575
    timbre = 0.02
    morph = range_hs (0.55,0.9) (lfNoise1 'α' KR 0.3) -- filter resonance
in X.miPlaits AR 62 9 harm timbre morph 0 0 0 0 0 0.5 0.5 * 0.25

-- MiPlaits ; something...
let trigger = dust 'α' KR (range_hs (0.1,7) (lfNoise2 'β' KR 0.1))
    harmonics = sinOsc KR 0.03 0 * 0.5 + 0.5
    timbre = lfTri KR 0.07 0 * 0.5 + 0.5
    morph = lfTri KR 0.11 0 * 0.5 + 0.5
    pitch_ = tiRand 'γ' 24 48 trigger
    engine = roundE (tRand 'δ' 0 15 trigger)
    sub = sinOsc AR (midiCPS pitch_) 0 * 0.1
    mi = X.miPlaits AR pitch_ engine harmonics timbre morph trigger 0 0 0 0 0.8 0.2 * 0.5
in (mi + sub) * 0.2

