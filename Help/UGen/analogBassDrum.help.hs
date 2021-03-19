-- analogBassDrum
let trig = dust2 'α' KR 8
    infsustain = 0.0
    accent = 0.25
    freq = tExpRand 'β' 40 120 trig
    tone = tRand 'γ' 0.0 0.35 trig
    decay = 0.15
    attackfm = tRand 'δ' 0.1 0.2 trig
    selffm = tRand 'ε' 0.1 0.9 trig
    sig = X.analogBassDrum AR trig infsustain accent freq tone decay attackfm selffm
in pan2 sig (tRand 'ζ' (-1) 1 trig) 1

-- analogBassDrum ; event control
let f _ (g,x,y,z,o,rx,ry,_,_,_) =
      let freq = midiCPS (x * 25 + 24)
          tr = trig g controlDur
          selffm = tRand 'α' 0.1 0.9 tr
          sig = X.analogBassDrum AR tr 0 z freq y rx (ry `in_range` (0.1,0.2)) selffm
      in pan2 sig (o * 2 - 1) 1
in mix (rEventVoicer 16 f) * control KR "gain" 2
