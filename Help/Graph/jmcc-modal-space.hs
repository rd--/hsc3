-- modal space, using local buffer (jmcc) #8
let ms1 n r =
      let b = asLocalBuf 'α' [0,2,3.2,5,7,9,10] {- dorian scale -}
          x = mouseX KR 0 15 Linear 0.1 {- mouse indexes into scale -}
          k = degreeToKey b x 12 {- 12 notes per octave -}
          o = sinOsc AR (midiCPS (r + k + n * 0.04)) 0 * 0.1
          t = lfPulse AR (midiCPS (mce2 48 55)) 0 0.15
          f = midiCPS (sinOsc KR 0.1 0 * 10 + r)
          d = rlpf t f 0.1 * 0.1
          m = o + d
      in combN m 0.31 0.31 2 + m
in ms1 (lfNoise1 'β' KR 3) 48 + ms1 (lfNoise1 'γ' KR 3) 72 * 0.25
