-- sos ; same as twopole
let theta = line kr (0.2 * pi) pi 5 RemoveSynth
    rho = line kr 0.6 0.99 5 RemoveSynth
    b1 = 2 * rho * cos theta
    b2 = - (rho * rho)
in sos (lfSaw ar 200 0 * 0.1) 1 0 0 b1 b2

-- sos ; http://www.earlevel.com/main/2011/01/02/biquad-formulas/
let fc = 100
    sr = 48000
    k = tan (pi * fc / sr)
    q = 0.707
    norm = 1 / (1 + k / q + k * k)
    a0 = k * k * norm
    a1 = 2 * a0
    a2 = a0
    b1 = 2 * (k * k - 1) * norm
    b2 = (1 - k / q + k * k) * norm
in sos (whiteNoiseId 'α' ar * 0.2) a0 a1 a2 (- b1) (- b2)

-- sos
let b1 = mouseY kr 1.45 1.998 Linear 0.2
    b2 = mouseX kr (-0.999) (-0.9998) Linear 0.2
in sos (impulse ar 2 0) 0.0 0.05 0.0 b1 b2

-- sos
let f (_,w,x,y,z,o,_,_,_,_,_) =
      let b1 = linLin y 0 1 1.45 1.998
          b2 = linLin x 0 1 (-0.999) (-0.9998)
      in pan2 (sos (trig (k2a w) 0) 0.0 0.05 0.0 b1 b2) (o * 2 - 1) 8
in mix (eventVoicer 16 f) * control kr "gain" 1

-- sos
let f (_,w,x,y,z,o,_,_,_,_,_) =
      let freq = midiCps (x * 25 + 48)
          rq = 0.002 + (y * 0.004)
          b1 = 1.987 * 0.9889999999 * cos 0.09
          b2 = -0.998057
          im = decay (trig1 (k2a w) sampleDur) 0.1
          s1 = sos im 0.3 0.0 0.0 b1 b2
          s2 = rhpf (s1 * 0.8) freq rq
          s3 = s2 + delayC (rhpf (s1 * 0.9) (freq * 0.99999) (rq * 0.999)) 0.02 0.01223
          s4 = decay2 s3 0.4 0.3 * s2
      in pan2 s4 (o * 2 - 1) 0.1
in mix (eventVoicer 16 f) * control kr "gain" 0.5
