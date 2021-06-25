-- harmonicOsc
let modulator = sinOsc KR 0.1 0
    freq = modulator `in_exprange` (10,1000)
    firstharmonic = 3
    amplitudes = X.rRandN 16 'β' 0.01 0.1
    sig = X.harmonicOsc AR freq firstharmonic amplitudes
in pan2 sig modulator 0.2

-- harmonicOsc ; event control
let f _ (g,x,_,z,o,_,_,_,_,_) =
      let amplitudes = X.rtRandN 16 'α' 0.01 0.1 g
      in pan2 (X.harmonicOsc AR (midiCPS (x * 25 + 36)) 1 amplitudes) (o * 2 - 1) (g * z)
in mix (eventVoicer 16 f) * control KR "gain" 0.5
