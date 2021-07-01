-- pond life (jmcc) #1 ; texture=overlap,8,8,4,inf
let f0 = 20 + rand 0 30
    f1 = fSinOsc kr f0 0 * rand 100 400 + linRand 500 2500 0
    a = lfPulse kr (3 / rand 1 9) 0 (rand 0.2 0.5) * 0.04
in pan2 (sinOsc ar f1 0 * a) (rand (-1) 1) 0.5

-- pond life (jmcc) #1 ; texture=overlap,8,8,4,inf ; id
let f0 = 20 + randId 'α' 0 30
    f1 = fSinOsc kr f0 0 * randId 'β' 100 400 + linRandId 'γ' 500 2500 0
    a = lfPulse kr (3 / randId 'δ' 1 9) 0 (randId 'ε' 0.2 0.5) * 0.04
in pan2 (sinOsc ar f1 0 * a) (randId 'ζ' (-1) 1) 0.5

-- pond life (jmcc) #1 ; event control
let f _ (g,x,y,z,o,rx,ry,_,_,_) =
      let f0 = 20 + tRand 0 (rx * 30) g
          f1 = fSinOsc kr f0 0 * linLin x 0 1 100 400 + linLin y 0 1 500 2500
          a = lfPulse kr (3 / linLin ry 0 1 1 9) 0 (rand 0.2 0.5)
      in pan2 (sinOsc ar f1 0 * a * 0.25) (o * 2 - 1) (z * g)
in mix (eventVoicer 16 f) * control kr "gain" 1

-- pond life (jmcc) #1 ; event control ; id
let f c (g,x,y,z,o,rx,ry,_,_,_) =
      let f0 = 20 + tRandId 'α' 0 (rx * 30) g
          f1 = fSinOsc kr f0 0 * linLin x 0 1 100 400 + linLin y 0 1 500 2500
          a = lfPulse kr (3 / linLin ry 0 1 1 9) 0 (randId (c,'β') 0.2 0.5)
      in pan2 (sinOsc ar f1 0 * a * 0.25) (o * 2 - 1) (z * g)
in mix (eventVoicer 16 f) * control kr "gain" 1
