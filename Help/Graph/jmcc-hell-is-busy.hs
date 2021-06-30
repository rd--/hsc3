-- hell is busy (jmcc) #1 ; texture=overlap,4,4,8,inf
let o = fSinOsc ar (400 + rand 0 2000) 0
    a = lfPulse kr (1 + rand 0 10.0) 0 (rand 0 0.7) * 0.04
in pan2 (o * a) (rand (-1) 1) 1

-- hell is busy (jmcc) #1 ; texture=overlap,4,4,8,inf ; id
let o = fSinOsc ar (400 + randId 'α' 0 2000) 0
    a = lfPulse kr (1 + randId 'β' 0 10.0) 0 (randId 'γ' 0 0.7) * 0.04
in pan2 (o * a) (randId 'δ' (-1) 1) 1

-- hell is busy (jmcc) #1 ; event control
let f _ (g,x,y,z,o,rx,_,_,_,_) =
      let s1 = fSinOsc ar (400 + x * 2000) 0
          s2 = lfPulse kr (1 + y * 10) 0 (rx * 0.7)
      in pan2 (s1 * s2 * 0.35) (o * 2 - 1) (z * g)
in mix (eventVoicer 16 f) * control kr "gain" 1
