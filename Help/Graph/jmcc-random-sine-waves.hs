-- random sine waves (jmcc) #1 ; texture=overlap,2,5,12,inf
let o = fSinOsc ar (rand 0 2000) 0 * 0.02
in pan2 o (rand2 1) 1

-- random sine waves (jmcc) #1 ; texture=overlap,2,5,12,inf ; id
let f = randId 'α' 0 2000
    o = fSinOsc ar f 0 * 0.02
in pan2 o (randId 'β' (-1) 1) 1

-- random sine waves (jmcc) #1 ; event control
let f (_,g,x,_,z,o,_,_,_,_,_) =
      let s = fSinOsc ar (x * 2000) 0
      in pan2 s (o * 2 - 1) (z * g)
in mix (eventVoicer 16 f) * control kr "gain" 0.25
