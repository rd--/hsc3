-- harmonicOsc
let modulator = sinOsc kr 0.1 0
    freq = modulator `in_exprange` (10,1000)
    firstharmonic = 3
    amplitudes = X.rRandNId 16 'β' 0.01 0.1
    sig = X.harmonicOsc ar freq firstharmonic amplitudes
in pan2 sig modulator 0.2

-- harmonicOsc ; event control
let f _ (g,x,_,z,o,_,_,_,_,_) =
      let amplitudes = X.rtRandNId 16 'α' 0.01 0.1 g
      in pan2 (X.harmonicOsc ar (midiCps (x * 25 + 36)) 1 amplitudes) (o * 2 - 1) (g * z)
in mix (eventVoicer 16 f) * control kr "gain" 0.5
