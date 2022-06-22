-- analogBassDrum
let trig = dust2Id 'α' kr 8
    infsustain = 0.0
    accent = 0.25
    freq = tExpRandId 'β' 40 120 trig
    tone = tRandId 'γ' 0.0 0.35 trig
    decay = 0.15
    attackfm = tRandId 'δ' 0.1 0.2 trig
    selffm = tRandId 'ε' 0.1 0.9 trig
    sig = X.analogBassDrum ar trig infsustain accent freq tone decay attackfm selffm
in pan2 sig (tRandId 'ζ' (-1) 1 trig) 1

-- analogBassDrum ; event control
let f (_,g,x,y,z,o,rx,ry,_,_,_) =
      let freq = midiCps (x * 25 + 24)
          tr = trig g controlDur
          selffm = tRandId 'α' 0.1 0.9 tr
          sig = X.analogBassDrum ar tr 0 z freq y rx (ry `in_range` (0.1,0.2)) selffm
      in pan2 sig (o * 2 - 1) 1
in mix (eventVoicer 16 f) * control kr "gain" 2
