-- pond life (jmcc) #1 ; texture=overlap,8,8,4,inf
let f0 = 20 + rand 'α' 0 30
    f1 = fSinOsc KR f0 0 * rand 'β' 100 400 + linRand 'γ' 500 2500 0
    a = lfPulse KR (3 / rand 'δ' 1 9) 0 (rand 'ε' 0.2 0.5) * 0.04
in pan2 (sinOsc AR f1 0 * a) (rand 'ζ' (-1) 1) 0.5

-- pond life (jmcc) #1 ; event control
let f c (g,x,y,z,o,rx,ry,_,_,_) =
      let f0 = 20 + tRand 'α' 0 (rx * 30) g
          f1 = fSinOsc KR f0 0 * linLin x 0 1 100 400 + linLin y 0 1 500 2500
          a = lfPulse KR (3 / linLin ry 0 1 1 9) 0 (rand (c,'β') 0.2 0.5)
      in pan2 (sinOsc AR f1 0 * a * 0.25) (o * 2 - 1) (z * g)
in mix (eventVoicer 16 f) * control KR "gain" 1
