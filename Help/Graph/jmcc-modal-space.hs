-- modal space (jmcc) #8 ; local buffer
let ms1 n r =
      let b = asLocalBuf [0,2,3.2,5,7,9,10] {- dorian scale -}
          x = mouseX kr 0 15 Linear 0.1 {- mouse indexes into scale -}
          k = degreeToKey b x 12 {- 12 notes per octave -}
          o = sinOsc ar (midiCps (r + k + n * 0.04)) 0 * 0.1
          t = lfPulse ar (midiCps (mce2 48 55)) 0 0.15
          f = midiCps (sinOsc kr 0.1 0 * 10 + r)
          d = rlpf t f 0.1 * 0.1
          m = o + d
      in combN m 0.31 0.31 2 + m
in ms1 (lfNoise1 kr 3) 48 + ms1 (lfNoise1 kr 3) 72 * 0.25

-- modal space (jmcc) #8 ; local buffer ; id
let ms1 n r =
      let b = asLocalBufId 'α' [0,2,3.2,5,7,9,10] {- dorian scale -}
          x = mouseX kr 0 15 Linear 0.1 {- mouse indexes into scale -}
          k = degreeToKey b x 12 {- 12 notes per octave -}
          o = sinOsc ar (midiCps (r + k + n * 0.04)) 0 * 0.1
          t = lfPulse ar (midiCps (mce2 48 55)) 0 0.15
          f = midiCps (sinOsc kr 0.1 0 * 10 + r)
          d = rlpf t f 0.1 * 0.1
          m = o + d
      in combN m 0.31 0.31 2 + m
in ms1 (lfNoise1Id 'β' kr 3) 48 + ms1 (lfNoise1Id 'γ' kr 3) 72 * 0.25

-- modal space (jmcc) #8 ; event control
let f (_,g,x,y,z,_,_,_,_,_,_) =
      let ms1 n r =
            let b = asLocalBuf [0,2,3.2,5,7,9,10] {- dorian scale -}
                k = degreeToKey b (x * 25) 12 {- 12 notes per octave -}
                o = sinOsc ar (midiCps (r + k + n * y * 0.08)) 0 * 0.1
                t = lfPulse ar (midiCps (mce2 48 55)) 0 0.15
                d = rlpf t (midiCps (sinOsc kr 0.1 0 * 10 + r)) 0.1 * 0.1
                m = o + d
            in combN m 0.31 0.31 2 + m
          ms = ms1 (lfNoise1 kr 3) 48 + ms1 (lfNoise1 kr 3) 72
      in ms * z * lagUD g 0.2 2
in mix (eventVoicer 16 f) * control kr "gain" 1

-- modal space (jmcc) #8 ; event control ; id
let f (c,g,x,y,z,_,_,_,_,_,_) =
      let ms1 n r =
            let b = asLocalBufId 'α' [0,2,3.2,5,7,9,10] {- dorian scale -}
                k = degreeToKey b (x * 25) 12 {- 12 notes per octave -}
                o = sinOsc ar (midiCps (r + k + n * y * 0.08)) 0 * 0.1
                t = lfPulse ar (midiCps (mce2 48 55)) 0 0.15
                d = rlpf t (midiCps (sinOsc kr 0.1 0 * 10 + r)) 0.1 * 0.1
                m = o + d
            in combN m 0.31 0.31 2 + m
          ms = ms1 (lfNoise1Id (c,'β') kr 3) 48 + ms1 (lfNoise1Id (c,'γ') kr 3) 72
      in ms * z * lagUD g 0.2 2
in mix (eventVoicer 16 f) * control kr "gain" 1

-- modal space (jmcc) #8 ; event control ; modeless
let f (_,g,_,y,z,_,_,_,p,_,_) =
      let ms1 n r =
            let o = sinOsc ar (midiCps (p * 127 + r + n * y * 0.08)) 0 * 0.1
                t = lfPulse ar (midiCps (mce2 48 55)) 0 0.15
                d = rlpf t (midiCps (sinOsc kr 0.1 0 * 10 + r)) 0.1 * 0.1
                m = o + d
            in combN m 0.31 0.31 2 + m
          ms = ms1 (lfNoise1 kr 3) 0 + ms1 (lfNoise1 kr 3) 24
      in ms * z * lagUD g (y * 0.05 + 0.01) (y * 2 + 1)
in mix (eventVoicer 16 f) * control kr "gain" 1

-- modal space (jmcc) #8 ; event control ; modeless ; id
let f (c,g,_,y,z,_,_,_,p,_,_) =
      let ms1 n r =
            let o = sinOsc ar (midiCps (p * 127 + r + n * y * 0.08)) 0 * 0.1
                t = lfPulse ar (midiCps (mce2 48 55)) 0 0.15
                d = rlpf t (midiCps (sinOsc kr 0.1 0 * 10 + r)) 0.1 * 0.1
                m = o + d
            in combN m 0.31 0.31 2 + m
          ms = ms1 (lfNoise1Id (c,'β') kr 3) 0 + ms1 (lfNoise1Id (c,'γ') kr 3) 24
      in ms * z * lagUD g (y * 0.05 + 0.01) (y * 2 + 1)
in mix (eventVoicer 16 f) * control kr "gain" 1
