-- dwgPlucked2 ; self deleting
let freq = 440
    amp = 0.5
    gate_ = 1
    c3 = 20
    inp = let e = envelope [0,1,1,0] [0.001,0.006,0.0005] (map EnvNum [5,-5,-8])
          in amp * lfClipNoiseId 'α' ar 2000 * envGen ar gate_ 1 0 1 DoNothing e
    ps = X.dwgPlucked2 ar freq amp gate_ 0.1 1 c3 inp 0.1 1.008 0.55 0.01
    pan = 0
    z = detectSilence ps 0.001 0.1 RemoveSynth
in mrg2 (pan2 ps pan 0.1) z

-- dwgPlucked2 ; re-sounding
let dur = mce [1,1,2,1,1,1,2,3,1,1,1,1,2,3,4] * 0.175
    sequId e s tr = demand tr 0 (dseqId e dinf (mce s))
    t = let d = dseqId 'α' dinf dur
        in tDuty ar d 0 DoNothing 1 0
    freq = let n0 = sequId 'β' [60,62,63,58,48,55] t
               n1 = sequId 'γ' [63,60,48,62,55,58] t
           in midiCps (mce2 n0 n1)
    amp = tRandId 'δ' 0.01 0.35 t -- pulse amplitude (0  - 1, def = 0.5)
    gate_ = 1 -- synth release
    pos = tRandId 'ε' 0.05 0.25 t -- pluck position (0 - 1, def = 0.14)
    c1 = 1 / tRandId 'ζ' 0.25 1.75 t -- reciprocal of decay time (def = 1.0)
    c3 = tRandId 'η' 10 1400 t -- high frequency loss factor (def = 30)
    inp = let e_dt = tRandId 'θ' 0.05 0.150 t
              env = decay2 t 0.001 e_dt * lfClipNoiseId 'ι' ar 2000
          in amp * lfClipNoiseId 'κ' ar 2000 * env -- pluck signal
    release = tRandId 'λ' 0.05 0.15 t -- release time (seconds, def = 0.1)
    mistune = tRandId 'μ' 0.992 1.008 t -- factor for detuning second string (def = 1.008)
    mp = tRandId 'ν' 0.35 0.65 t -- excitation mixer (def = 0.55)
    gc = tRandId 'ξ' 0.001 0.020 t -- coupling string factor (def = 0.01)
    ps = X.dwgPlucked2 ar freq amp gate_ pos c1 c3 inp release mistune mp gc
    pan = tRandId 'ο' (-1) 1 t
in pan2 ps pan 0.1

-- dwgPlucked2 ; event control
let f (_,g,x,y,z,o,rx,_,p,_,_) =
      let c3 = 20
          (gt,tr) = eventGateReset g p
          dx = latch x tr - x
          freq = midiCps (p + dx)
          inp = let e = envelope [0,1,1,0] [0.001,0.006,0.0005] (map EnvNum [5,-5,-8])
                in z * lfClipNoiseId 'α' ar 2000 * envGen ar gt 1 0 1 DoNothing e
          ps = X.dwgPlucked2 ar freq 1 y 0.1 1 c3 inp 0.1 (1 + rx * 0.05) 0.55 (0.1 * z)
      in pan2 ps (o * 2 - 1) 1
in mix (eventVoicer 16 f) * control kr "gain" 0.75
