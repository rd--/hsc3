-- random sine waves (jmcc) #1 ; texture=overlap,2,5,12,inf
let f = rand 'α' 0 2000
    o = fSinOsc AR f 0 * 0.02
in pan2 o (rand 'β' (-1) 1) 1

-- random sine waves (jmcc) #1 ; event control
let f _ (g,x,_,z,o,_,_,_) =
      let s = fSinOsc AR (x * 2000) 0
      in pan2 s (o * 2 - 1) (z * g)
in mix (rEventVoicer 16 f) * control KR "gain" 0.5
