-- neoFormant
let modulator = sinOsc KR 0.1 0
    formantfreq = 150
    carrierfreq = lfNoise2 'α' KR 10 `in_exprange` (100,550)
    phaseshift = modulator `in_range` (0,1)
    sig = X.neoFormant AR formantfreq carrierfreq phaseshift
in pan2 sig modulator 0.1

-- neoFormant ; event control
let f _ (g,x,y,z,o,rx,_,_,_,_) =
      let carrierFreq = midiCPS (x * 13 + 36)
          formantFreq = y `in_exprange` (100,550)
          phaseShift = lag rx 0.5 * 2
      in pan2 (X.neoFormant AR formantFreq carrierFreq phaseShift) (o * 2 - 1) (lagUD (z * g) 0 0.2)
in mix (eventVoicer 16 f) * control KR "gain" 0.2
