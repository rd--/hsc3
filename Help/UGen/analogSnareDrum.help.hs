-- analogSnareDrum
let trig = dust2 'α' KR 8
    infsustain = 0.0
    accent = 0.25
    freq = tExpRand 'β' 40 220  trig
    tone = tRand 'γ' 0.0 0.4 trig
    decay = tRand 'δ' 0.1 0.8 trig
    snappy = tRand 'ε' 0.0 0.9  trig
    sig = X.analogSnareDrum AR trig infsustain accent freq tone decay snappy
in pan2 sig (tRand 'ζ' (-1) 1 trig) 0.1

-- analogSnareDrum ; event control
let f _ (g,x,y,z,o,rx,ry,_,_,_) =
      let freq = midiCPS (x * 25 + 36)
          tr = trig g controlDur
          sig = X.analogSnareDrum AR tr 0 z freq y rx (ry * 2)
      in pan2 sig (o * 2 - 1) (lagUD g 0 2)
in mix (eventVoicer 16 f) * control KR "gain" 0.25
