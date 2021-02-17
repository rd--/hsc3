-- lpf
let f = xLine KR 0.7 300 20 RemoveSynth
    ff = fSinOsc KR f 0 * 3600 + 4000
in lpf (saw AR 200 * 0.1) ff

-- lpf ; control rate filtering
let ctl = lpf (lfPulse KR 8 0 0.5) (mouseX KR 2 50 Exponential 0.1)
in sinOsc AR (ctl * 200 + 400) 0 * 0.1

-- lpf ; event control
let f _ (g,_,y,z,o,rx,ry,p,_,_) =
      let f0 = midiCPS p
      in pan2 (lpf (blip AR f0 (1 + y * 5)) (f0 * 0.25) * 4) (o * 2 - 1) (lagUD g 0.05 1 * z)
in mix (rEventVoicer 16 f) * control KR "gain" 2
